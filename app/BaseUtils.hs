{-# LANGUAGE TupleSections #-}
module BaseUtils (module BaseUtils, module ErrOr) where

import ErrOr

import Numeric.Natural (Natural)
import Data.Char (toUpper, toLower)
import Text.Pretty.Simple (pShowLightBg)
import Data.Text.Lazy (unpack)



data RTree a = Leaf | Node a [RTree a] deriving Show

ppRTree :: Show a => RTree a -> String
ppRTree = ppRTree' 0 where
  ppRTree' pfx Leaf = "\n" ++ prefix pfx "Leaf"
  ppRTree' pfx (Node x xs) = "\n" ++
    prefix pfx ("Node "
                ++ show x
                ++ " ["
                ++ intercalate' ",\n" (map (ppRTree' (inc pfx)) xs))
                ++ "]"

isLeaf :: RTree a -> Bool
isLeaf Leaf = True
isLeaf _ = False

isNode :: RTree a -> Bool
isNode (Node _ _) = True
isNode _ = False

getValue :: RTree a -> Maybe a
getValue (Node x _) = Just x
getValue Leaf = Nothing

toList :: RTree a -> [a]
toList (Node x xs) = x : concatMap toList xs
toList Leaf = []

toInt :: Natural -> Int
toInt = fromInteger . toInteger

fromInt :: Int -> Natural
fromInt = fromInteger . toInteger

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y

trd3 :: (a,b,c) -> c
trd3 (_,_,z) = z

intercalate' :: String -> [String] -> String
intercalate' _ [] = ""
intercalate' _ [x] = x
intercalate' sep (x : xs) = x ++ sep ++ intercalate' sep xs

inc :: Natural -> Natural
inc = (+2)

pfxStr :: Natural -> String
pfxStr n = replicate (fromInteger (toInteger n)) ' '

prefix :: Natural -> String -> String
prefix n s = pfxStr n ++ s

parens' :: Char -> Char -> String -> String
parens' l r str = l : str ++ [r]

parens :: String -> String
parens = parens' '(' ')'

brackets :: String -> String
brackets = parens' '[' ']'

braces :: String -> String
braces = parens' '{' '}'

bracesNL :: Natural -> Natural -> String -> String
bracesNL bf af str = pfxStr bf ++ "{\n" ++ str ++ "\n" ++ pfxStr af ++ "}"

firstUpper :: String -> String
firstUpper [] = undefined
firstUpper (c : cs) = toUpper c : cs

firstLower :: String -> String
firstLower [] = undefined
firstLower (c : cs) = toLower c : cs

spacer :: String
spacer = "\n\n"

dup :: a -> (a,a)
dup x = (x,x)

assoc :: (a,(b,c)) -> ((a,b),c)
assoc (x,(y,z)) = ((x,y),z)

ppShow :: Show a => a -> String
ppShow = unpack . pShowLightBg

ppPrint :: Show a => a -> IO ()
ppPrint = putStrLn . unpack . pShowLightBg

secondM :: Monad m => (b -> m b') -> (a,b) -> m (a,b')
secondM f (x,y) = fmap (x,) (f y)

singleton :: a -> [a]
singleton x = [x]
