{-# LANGUAGE TupleSections #-}
module Effpi where

import BaseUtils
import Core
import Utils
import Projection
import Scala

import System.FilePath ((</>))
import Numeric.Natural (Natural)
import Data.List (partition, nub, sort)
import Data.Bifunctor (first, second, bimap)
import Data.Tuple (swap)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (when, unless, foldM, (<=<))
import Data.List.Unique (allUnique)

baseImports :: [Import]
baseImports = ["effpi.process._",
               "effpi.process.dsl._",
               "effpi.channel.Channel",
               "effpi.channel.{InChannel, OutChannel}",
               "scala.concurrent.duration.Duration"]

caseClassLabels :: G () -> G ()
caseClassLabels (GComm p q v ks) = GComm p q v (map f ks) where
  f k = k {label = toCClass (label k), cont = caseClassLabels (cont k)}
caseClassLabels (GRec v k) = GRec v (caseClassLabels k)
caseClassLabels g = g

-- Probably better to use the State monad?
setUniqueIDs :: G () -> (Natural, G AnnotG)
setUniqueIDs = initIDs 0 0 [] where
  initIDs :: RVarSeed -> ChanSeed -> [Natural] -> G () -> (ChanSeed, G AnnotG)
  initIDs i j env (GComm p q _ ks) =
    second (GComm p q (AChan j [])) (foldl f (j+1, []) ks) where
    f :: (ChanSeed, [Choice G AnnotG]) -> Choice G ()
      -> (ChanSeed, [Choice G AnnotG])
    f (j', z) k = let g = initIDs i j' env (cont k) in
      second ((z ++) . singleton . (\g' -> k {cont = g'})) g
  initIDs i j env (GRec _ k) =
    second (GRec (ARVar i)) (initIDs (i+1) j (i : env) k)
  initIDs _ j env (GVar t _) = (j, GVar t (ARVar (env !! toInt t)) )
  initIDs _ j _ GEnd = (j, GEnd)

setFreshIDsKs :: ChanSeed -> [Choice S AnnotG] -> (ChanSeed, [Choice S AnnotG])
setFreshIDsKs i = foldr f (i, []) where
  f x (i', z) = second ((: z) . setK x) (setFreshIDs i' (cont x))

setFreshIDs :: ChanSeed -> S AnnotG -> (ChanSeed, S AnnotG)
setFreshIDs i (SSend p (AMChan xs) ks) =
  second (SSend p (AChan i xs)) (setFreshIDsKs (i+1) ks)
setFreshIDs i (SSend p v ks) = second (SSend p v) (setFreshIDsKs i ks)
setFreshIDs i (SRecv q (AMChan xs) ks) =
  second (SRecv q (AChan i xs)) (setFreshIDsKs (i+1) ks)
setFreshIDs i (SRecv q v ks) = second (SRecv q v) (setFreshIDsKs i ks)
setFreshIDs i (SRec v k) = second (SRec v) (setFreshIDs i k)
setFreshIDs i s = (i, s)

assnFreshIDs :: [(AnnotG, Natural)] -> S AnnotG -> ErrOr (S AnnotG)
assnFreshIDs env (SSend p v@(AMChan vs) ks) =
  case lookup v env of
    Nothing ->
      Err ("Error assigning IDs to merged channels -- no entry found in env: "
            ++ ppShow (v, env))
    Just i -> fmap (SSend p (AChan i vs)) (mapM (contM' (assnFreshIDs env)) ks)
assnFreshIDs env (SSend p v ks) =
  fmap (SSend p v) (mapM (contM' (assnFreshIDs env)) ks)
assnFreshIDs env (SRecv q v@(AMChan vs) ks) =
  case lookup v env of
    Nothing ->
      Err ("Error assigning IDs to merged channels -- no entry found in env: "
            ++ ppShow (v, env))
    Just i -> fmap (SRecv q (AChan i vs)) (mapM (contM' (assnFreshIDs env)) ks)
assnFreshIDs env (SRecv q v ks) =
  fmap (SRecv q v) (mapM (contM' (assnFreshIDs env)) ks)
assnFreshIDs env (SRec v k) = fmap (SRec v) (assnFreshIDs env k)
assnFreshIDs _ s = return s

indexBras :: S AnnotG -> ErrOr (S AnnotS)
indexBras = fmap snd . index 0 where
  indexChoice :: Natural -> Choice S AnnotG -> ErrOr (Natural, Choice S AnnotS)
  indexChoice i k = fmap (second (setK k)) (index i (cont k))

  index :: Natural -> S AnnotG -> ErrOr (Natural, S AnnotS)
  index i (SSend p (AChan j js) ks) =
    fmap (second (SSend p (ASChan j js Nothing))) (foldM f (i,[]) ks) where
      f (i', z) k = fmap (second (: z)) (indexChoice i' k)
  index i (SRecv q (AChan j js) [k]) =
    fmap (second (SRecv q (ASChan j js Nothing) . singleton)) (indexChoice i k)
  index i (SRecv q (AChan j js) ks) | length ks > 1 =
    fmap (second (SRecv q (ASChan j js (Just i)))) (foldM f (i+1,[]) ks) where
      f (i', z) k = fmap (second (: z)) (indexChoice i' k)
  index i (SRec (ARVar j) k) = fmap (second (SRec (ASRVar j))) (index i k)
  index i (SVar t (ARVar j)) = return (i, SVar t (ASRVar j))
  index i SEnd = return (i, SEnd)

  index i s = Err ("Unexpected case in indexBras: " ++ ppShow (i,s))

getAnnots :: S a -> [a]
getAnnots (SSend _ v ks) = v : concatMap (getAnnots . cont) ks
getAnnots (SRecv _ v ks) = v : concatMap (getAnnots . cont) ks
getAnnots (SRec v k) = v : getAnnots k
getAnnots _ = []

getAnnotsAndLabels :: S a -> [(a, [Label])]
getAnnotsAndLabels (SSend _ v ks) =
  (v, map label ks) : concatMap (getAnnotsAndLabels . cont) ks
getAnnotsAndLabels (SRecv _ v ks) =
  (v, map label ks) : concatMap (getAnnotsAndLabels . cont) ks
getAnnotsAndLabels (SRec v k) = (v, []) : getAnnotsAndLabels k
getAnnotsAndLabels _ = []

toRecVar :: (AnnotS, [Label]) -> ErrOr RecVar
toRecVar (ASRVar c, _) = Ok (MkRecVar c (mkRecVarStr c))
toRecVar x = Err ("Unexpected case in toRecVar: " ++ show x)

toChanMap :: (AnnotS, [Label]) -> ErrOr ChanMap
toChanMap (ASChan c cs _, ls)
  | null (rmCrashLabs ls) =
    Err ("Channels must have a non-crash carrier type; do you have a pure crash recovery branch? (AChan: " ++ show c ++ ")")
  | otherwise = Ok (if null cs then BaseChan c ls else MergeChan c cs ls)
toChanMap x = Err ("Unexpected case in toChanMap: " ++ show x)

chanIDs :: S AnnotS -> [(Natural, Natural -> [Label] -> ChanUB)]
chanIDs (SSend _ (ASChan c _ _) ks) =
  (c, OutChan) : concatMap (chanIDs . cont) ks
chanIDs (SRecv _ (ASChan c _ _) ks) =
  (c, InChan) : concatMap (chanIDs . cont) ks
chanIDs (SRec _ k) = chanIDs k
chanIDs (SVar _ _) = []
chanIDs SEnd = []
chanIDs s = error ("Unexpected case in chanIDs: " ++ show s)

lookupChan :: Natural -> [ChanMap] -> ChanMap
lookupChan i [] = error ("Channel id " ++ show i ++ " not found.")
lookupChan i (c@(BaseChan j _) : cs)
  | i == j = c
  | otherwise = lookupChan i cs
lookupChan i (c@(MergeChan j _ _) : cs)
  | i == j = c
  | otherwise = lookupChan i cs

toChUB :: (Natural -> [Label] -> ChanUB) -> ChanMap -> ChanUB
toChUB ctr (BaseChan i ls) = ctr i ls
toChUB ctr (MergeChan i _ ls) = ctr i ls

fromChMap :: [ChanMap] -> S AnnotS -> [ChanUB]
fromChMap cs s =
  sort (map (uncurry toChUB . swap . first (`lookupChan` cs)) (chanIDs s))

lookupChUB :: Natural -> [ChanUB] -> ChanUB
lookupChUB i [] = error ("Channel upper bound id " ++ show i ++ " not found.")
lookupChUB i (c@(InChan j _) : cs)
  | i == j = c
  | otherwise = lookupChUB i cs
lookupChUB i (c@(OutChan j _) : cs)
  | i == j = c
  | otherwise = lookupChUB i cs

fromChUB :: [ChanUB] -> S AnnotS -> [ChanUB]
fromChUB cs s = filter (f (map fst (chanIDs s))) cs where
  f ids (InChan i _) = i `elem` ids
  f ids (OutChan i _) = i `elem` ids

mkRecVarStr :: Natural -> String
mkRecVarStr = ("RecT" ++) . show

fromSAnnotS :: [ChanUB] -> Role -> S AnnotS -> (TyBody, [TyDecl])
fromSAnnotS cs r (SSend _ (ASChan i _ Nothing) [k]) =
  case lookupChUB i cs of
    c@(OutChan _ _) ->
      first (Out c (label k) (payload k)) (fromSAnnotS cs r (cont k))
    InChan _ _ -> error "Needed OutChan for Out, got InChan"
fromSAnnotS cs r (SSend _ (ASChan i _ Nothing) ks) | length ks > 1 =
  case lookupChUB i cs of
    c@(OutChan _ _) -> first (Sel c) (foldr f ([], []) ks) where
      f :: Choice S AnnotS -> ([(Label,B,TyBody)],[TyDecl])
        -> ([(Label,B,TyBody)],[TyDecl])
      f k (z, ds) =
        bimap ((: z) . (label k,payload k,)) (ds ++) (fromSAnnotS cs r (cont k))
    InChan _ _ -> error "Needed OutChan for Sel, got InChan"
fromSAnnotS _ _ (SRecv _ (ASChan _ _ Nothing) [k])
  | isCrashLabel (label k) =
    error $
      "\nCongratulations! You have found a pure crash recovery operation."
      ++ " This is probably a bad thing; it's certainly unexpected."
      ++ spacer ++ show k
fromSAnnotS cs r (SRecv _ (ASChan i _ Nothing) [k])
  | not (isCrashLabel (label k)) =
    case lookupChUB i cs of
      c@(InChan _ _) ->
        first (flip (In c (label k)) Nothing) (fromSAnnotS cs r (cont k))
      OutChan _ _ -> error "Needed InChan for In, got OutChan"
fromSAnnotS cs r (SRecv _ (ASChan i _ (Just j)) ks)
  | length ks > 1 && not (any (isCrashLabel . label) ks) =
    case lookupChUB i cs of
      c@(InChan _ _) ->
        let xs = sort (concatMap (fromChUB cs . cont) ks)
            ls = filter (not .isCrashLabel) (map label ks)
            hd = Bra c j ls xs Nothing
            na = show r ++ show j
            f :: Choice S AnnotS -> ((Label, TyBody), [TyDecl])
            f  = assoc . bimap label (fromSAnnotS cs r . cont) . dup
            tl = uncurry (:) (bimap (Decl j na xs) concat (unzip (map f ks)))
        in (hd, tl)
      OutChan _ _ -> error "Needed InChan for Bra, got OutChan"
fromSAnnotS cs r (SRecv _ (ASChan i _ (Just j)) ks)
  | length ks > 1 && any (isCrashLabel . label) ks =
    case lookupChUB i cs of
      c@(InChan _ _) ->
        case second head (partition (not . isCrashLabel . label) ks) of
          ([k], ck) ->
            let (cb, ds) = fromSAnnotS cs r (cont ck)
                f = flip (In c (label k)) (Just cb)
            in bimap f (ds ++) (fromSAnnotS cs r (cont k))
          (ks', ck) ->
            let (cb, ds) = fromSAnnotS cs r (cont ck)
                xs = sort (concatMap (fromChUB cs . cont) ks')
                ls = filter (not . isCrashLabel) (map label ks)
                f :: Choice S AnnotS -> ((Label, TyBody), [TyDecl])
                f  = assoc . bimap label (fromSAnnotS cs r . cont) . dup
                g  = Decl j (show r ++ show j) xs
                h  = (ds ++) . concat
            in (Bra c j ls xs (Just cb),
                uncurry (:) (bimap g h (unzip (map f ks'))))
      OutChan _ _ -> error "Needed InChan for Bra, got OutChan"
fromSAnnotS cs r (SRec (ASRVar i) k) =
  first (Rec (MkRecVar i (mkRecVarStr i))) (fromSAnnotS cs r k)
fromSAnnotS _ _ (SVar _ (ASRVar i)) = (Var (MkRecVar i (mkRecVarStr i)), [])
fromSAnnotS _ _ SEnd = (End, [])
fromSAnnotS _ _ s = error ("\nUnexpected case in fromSAnnotS: " ++ show s)

toRoleTy :: [ChanMap] -> Role -> S AnnotS -> RoleTy
toRoleTy cs r s =
  let cs' = fromChMap cs s
  in uncurry (MkRoleTy (show r) cs') (fromSAnnotS cs' r s)

toRoleChans :: RoleTy -> (String, [ChanUB])
toRoleChans (MkRoleTy n cs _ _) = (n, cs)

effpiG :: ProtocolName -> G () -> ErrOr Effpi
effpiG _ g | "C" `elem` map show (participants g) =
  Err $ "Potential name conflict between Roles and Channels: "
     ++ "participants of given global type contain 'C'. "
     ++ "This will be fixed in the future, but please change it for now."
effpiG name g = do
  let (i, g') = setUniqueIDs (caseClassLabels g)
  caseCs <- genCaseClasses g'
  locals <- setFreshIDs' i g'
  let annots = concatMap (getAnnotsAndLabels . snd) locals
  let allChs = sort (map (second rmCrashLabs) (filter (isASChan . fst) annots))
  let mgdChs = filter (isMergedChan . fst) allChs
  let topChs = getTopChans mgdChs allChs
  muVars <- fmap nub (mapM toRecVar (filter (isASRVar . fst) annots))
  topChMap <- mapM toChanMap topChs
  allChMap <- mapM toChanMap allChs
  let roleTys = map (uncurry (toRoleTy allChMap)) locals
  let mainFn = MkMain topChMap (map toRoleChans roleTys)
  -- Err $ ppShow (i, g', locals !! 0)
  return $ Effpi name baseImports caseCs muVars roleTys mainFn
  where
    genCaseClasses g'
      | allUnique (map (\(MkCClass x _) -> x) casesG0) = Ok casesG0
      | otherwise =
        Err $ "Label and payload type names must be disjoint: "
            ++ show (map (\(MkCClass x _) -> x) casesG0)
      where
        pays :: [CClass]
        pays =
          map ((`MkCClass` Nothing) . show) (filter isCustomType (payloads g'))
        labs :: [CClass]
        labs = map f (labelsAndPayloads g') where
          f (l,BUnit) = MkCClass (show l) Nothing
          f (l,b) = MkCClass (show l) (Just (firstUpper (show b)))
        casesG0 = pays ++ labs

    setFreshIDs' i g' =
      let locals = projAllRoles g'
          cs = nub (filter isAMChan (concatMap (getAnnots . snd) locals))
          csMap = zip cs [i..]
      in mapM (secondM indexBras <=< secondM (assnFreshIDs csMap)) locals

    isMergedChan (ASChan _ (_ : _) _) = True
    isMergedChan _ = False

    getMgdIDs (ASChan _ ids _, _) = ids
    getMgdIDs _ = []

    hasBeenMerged mgd (ASChan i _ _) = i `elem` mgd
    hasBeenMerged _ _ = False

    getTopChans mgdChs allChs = nub (filter p allChs) where
        p = not . hasBeenMerged (concatMap getMgdIDs mgdChs) . fst

data Verbosity = Loud | Quiet | Silent deriving Show

isLoud :: Verbosity -> Bool
isLoud Loud = True
isLoud _ = False

isQuiet :: Verbosity -> Bool
isQuiet Quiet = True
isQuiet _ = False

isSilent :: Verbosity -> Bool
isSilent Silent = True
isSilent _ = False

effpiGIO :: Verbosity -> ProtocolName -> G () -> IO ()
effpiGIO verbosity name g
  | name == "EVoting" = do
    let fname = "scala/" </> (name ++ ".scala")
    case fmap ((++ "\n") . toScala') (effpiG name g) of
      Err err -> do
        unless (isQuiet verbosity)
          (putStrLn ("Error generating Scala: " ++ err))
        when (isQuiet verbosity) (error ("Error generating Scala: " ++ err))
      Ok outStr -> do
        when (isLoud verbosity) $ putStrLn outStr
        createDirectoryIfMissing False "scala"
        writeFile fname outStr      
  | otherwise = do
    let fname = "scala/" </> (name ++ ".scala")
    case fmap ((++ "\n") . toScala) (effpiG name g) of
      Err err -> do
        unless (isQuiet verbosity)
          (putStrLn ("Error generating Scala: " ++ err))
        when (isQuiet verbosity) (error ("Error generating Scala: " ++ err))
      Ok outStr -> do
        when (isLoud verbosity) $ putStrLn outStr
        createDirectoryIfMissing False "scala"
        writeFile fname outStr
