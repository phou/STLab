module Parser ( parseFile, testAll', testAll ) where

import BaseUtils (firstUpper, ErrOr (..), spacer)
import IR
import ParserUtils

import Text.ParserCombinators.Parsec
import Text.Parsec (endOfLine)
import Data.Maybe (fromMaybe)
import System.Directory (listDirectory)
import Data.List (isSuffixOf, sort)
import PPrinter (ppG)
import Core (G)

-- [Parser] -------------------------------------------------------------------

roleDecl :: GenParser Char st PRole
roleDecl = do
  spaces
  _ <- string "r"
  ctr <- f <|> g
  spaces
  str <- many1 (noneOf ")(,\n")
  return (ctr str)
  where
    f = do
      _ <- string "eliable role"
      return Reliable
    g = do
      _ <- string "ole"
      return Unreliable

roleArgs :: GenParser Char st [PRole]
roleArgs = sepBy roleDecl (char ',')

stmt :: GenParser Char st PStmt
stmt = do
  x <- ident
  spaces
  if x == "rec" then recStmt else
    if x == "continue" then tdkStmt else
      if x == "choice" then choiceStmt else
        if x == "crash" then commStmt x else
          commStmt (firstUpper x)
  where
    recStmt = do
      t <- ident
      whitespace
      b <- body
      whitespace
      return (Rec t b)

    tdkStmt = do
      t <- ident
      eos
      return (Var t)

    choiceStmt = do
      reserved "at"
      p <- ident
      whitespace
      b <- body
      try (reserved "or" >> choiceStmt' p [b]) <|> return (IRChoice p [b])

    choiceStmt' :: String -> [[PStmt]] -> GenParser Char st PStmt
    choiceStmt' p bs = do
        b <- body
        try (reserved "or" >> choiceStmt' p (b : bs)) <|> return (IRChoice p (reverse (b : bs)))

    checkPayload :: GenParser Char st Bool
    checkPayload = try (char '(') >> return True

    commStmt msg =
      do
        (arg, p, q) <- commStmtP <|> commStmtNP
        return (Comm msg (fromMaybe "" arg) p q)
      where
        commStmtP = do
          arg <- parens ident
          reserved "from"
          p <- ident
          reserved "to"
          q <- ident
          eos
          return (Just arg, p, q)
        commStmtNP = do
          reserved "from"
          p <- ident
          reserved "to"
          q <- ident
          eos
          return (Nothing, p, q)

body :: GenParser Char st [PStmt]
body = braces (whiteComments >> many1 stmt)

globalProtocol :: GenParser Char st PG
globalProtocol = do
  _ <- string "global protocol"
  whitespace
  f <- ident
  whitespace
  xs <- parens roleArgs
  whitespace
  MkPG f xs <$> body

pragma :: GenParser Char st ()
pragma = do
  _ <- char '('
  _ <- char '*'
  _ <- char '#'
  spaces
  _ <- ident
  spaces
  _ <- char '#'
  _ <- char '*'
  _ <- char ')'
  _ <- endOfLine
  return ()

pragmaProtocol :: GenParser Char st PG
pragmaProtocol = many pragma >> whiteComments >> globalProtocol

-- [External Interface] -------------------------------------------------------

parseFile :: String -> IO (ErrOr (G ()))
parseFile fname = do
  r <- parseFromFile pragmaProtocol fname
  case r of
    Left err -> return (Err (show err))
    Right pg -> return (fromPG pg)

testAll' :: Bool -> String -> IO ()
testAll' fg dir = do
  fs <- listDirectory dir
  mapM_ f (sort (filter (isSuffixOf ".nuscr") fs))
  where
    f fn = let fname = dir ++ "/" ++ fn in do
      r <- parseFromFile pragmaProtocol fname
      case r of
        Left err ->
          putStrLn ("File: " ++ fname) >> print err  >> putStr "\n\n"
        Right pg -> case fromPG pg of
          Err err ->
            putStrLn ("File: " ++ fname) >> putStr err >> putStr spacer
          Ok g ->
            putStrLn ("File: " ++ fname) >>
            putStrLn "Ok" >>
            if fg
              then ppG g >> putStr spacer
              else putStr spacer 

testAll :: String -> IO ()
testAll = testAll' False
