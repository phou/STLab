module Utils where

import Core
import BaseUtils

import Data.List (sort,sortBy,nub)

isReliable :: Role -> Bool
isReliable (MkRole _ _ R) = True
isReliable _ = False

participants :: G a -> [Role]
participants = sort . nub . roles where
  roles (GComm p q _ ks) = p : q : concatMap (roles . cont) ks
  roles (GRec _ k) = roles k
  roles _ = []

isCrashLabel :: Label -> Bool
isCrashLabel CrashLab = True
isCrashLabel _ = False

isCrashBranch :: Choice a b -> Bool
isCrashBranch = isCrashLabel . label

labels :: G a -> [Label]
labels = sort . nub . labels' where
  labels' (GComm _ _ _ ks) = concatMap f ks where
    f k | isCrashBranch k = labels' (cont k)
        | otherwise = label k : labels' (cont k)
  labels' (GRec _ k) = labels' k
  labels' _ = []

labelsAndPayloads :: G a -> [(Label, B)]
labelsAndPayloads = sortBy g . nub . lAPs where
  g x y = compare (fst x) (fst y)
  f k | isCrashBranch k = lAPs (cont k)
      | otherwise = (label k, payload k) : lAPs (cont k)

  lAPs (GComm _ _ _ ks) = concatMap f ks
  lAPs (GRec _ k) = lAPs k
  lAPs _ = []

viewFlatComms :: G a -> [(Role,Role,[Label])]
viewFlatComms = sort . nub . viewFlatComms' where
  viewFlatComms' (GComm p q _ ks) =
    let types = map label ks
    in (p,q,types) : concatMap (viewFlatComms' . cont) ks
  viewFlatComms' (GRec _ k) = viewFlatComms' k
  viewFlatComms' _ = []

isCustomType :: B -> Bool
isCustomType (BType _ _) = True
isCustomType _ = False

payloads :: G a -> [B]
payloads = sort . nub . payloads' where
  payloads' (GComm _ _ _ ks) =
    concatMap (\k -> payload k : payloads' (cont k)) ks
  payloads' (GRec _ k) = payloads' k
  payloads' _ = []

toCClass :: Label -> Label
toCClass = mapLabel firstUpper

rmCrashLabs :: [Label] -> [Label]
rmCrashLabs = filter (not . isCrashLabel) 

setK :: Choice ty a -> ty b -> Choice ty b
setK x k' = x {cont = k'}

cont' :: (ty a -> ty a) -> Choice ty a -> Choice ty a
cont' f x = setK x (f (cont x))

contM' :: Monad m => (ty a -> m (ty a)) -> Choice ty a -> m (Choice ty a)
contM' f x = fmap (setK x) (f (cont x))
