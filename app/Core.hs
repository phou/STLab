{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Core where

import Numeric.Natural ( Natural )
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data Reliability = R | U deriving (Show, Generic, NFData)
data Role = MkRole Natural String Reliability deriving (Generic, NFData)
data Label = MkLabel Natural String | CrashLab deriving (Generic, NFData)

instance Eq Role where
  (==) (MkRole i _ _) (MkRole j _ _) = i == j
instance Eq Label where
  (==) (MkLabel i _) (MkLabel j _) = i == j
  (==) CrashLab CrashLab = True
  (==) _ _ = False

instance Show Role where
  show (MkRole _ p _) = p
instance Show Label where
  -- show (MkLabel i l) = l ++ "_" ++ show i
  show (MkLabel _ l) = l
  show CrashLab = "crash"

mapLabel :: (String -> String) -> Label -> Label
mapLabel f (MkLabel i s) = MkLabel i (f s)
mapLabel _ CrashLab = CrashLab

instance Ord Role where
  compare (MkRole i _ _) (MkRole j _ _) = compare i j
instance Ord Label where
  compare (MkLabel i _) (MkLabel j _) = compare i j
  compare CrashLab (MkLabel _ _) = LT
  compare (MkLabel _ _) CrashLab = GT
  compare CrashLab CrashLab = EQ

data B = BInt | BReal | BString | BUnit | BBool | BType Natural String
       deriving (Eq, Ord, Generic, NFData)
data Choice ty a = Choice {
    label :: Label,
    payload :: B,
    cont :: ty a
  } deriving (Show, Generic, NFData)
data G a = GComm Role Role a [Choice G a]
         | GRec a (G a)
         | GVar Natural a
         | GEnd
         deriving Show
data S a = SRecv Role a [Choice S a]
         | SSend Role a [Choice S a]
         | SRec a (S a)
         | SVar Natural a
         | SEnd
         deriving (Show, Generic, NFData)

instance Show B where
  show BInt = "Int"
  show BReal = "Double"
  show BString = "String"
  show BUnit = "()"
  show BBool = "Boolean"
  show (BType _ ty) = ty

