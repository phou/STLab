{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main (main) where

import BaseUtils
import Core
import Projection
import Parser
import Effpi
import PPrinter

import System.Directory
import System.FilePath
import System.Console.CmdArgs
    ( Data,
      Typeable,
      (&=),
      cmdArgs,
      explicit,
      help,
      helpArg,
      name,
      program,
      summary,
      typDir,
      typFile,
      verbosityArgs,
      versionArg,
      isLoud,
      Default(def))
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when, unless)

data MyOptions = MyOptions {
    file    :: FilePath,
    outdir  :: FilePath,
    effpi   :: Bool,
    project :: Bool
  } deriving (Data, Typeable, Show, Eq)

-- Customize your options, including help messages, shortened names, etc.
myProgOpts :: MyOptions
myProgOpts = MyOptions {
    file = def &= typFile &= help "Parse a single nuScr file",
    outdir = "scala/" &= typDir &= help "Output directory for generated code",
    effpi = def &= help "Generate Effpi skeleton code",
    project = def &= help "Prints all local types; superseded by --effpi"
  }

getOpts :: IO MyOptions
getOpts = cmdArgs $ myProgOpts
    &= verbosityArgs [explicit, name "Verbose", name "v"] []
    &= versionArg [explicit, name "version", summary _PROGRAM_INFO]
    &= summary _PROGRAM_INFO
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME, _PROGRAM_VERSION, _PROGRAM_INFO, _PROGRAM_ABOUT :: String
_PROGRAM_NAME = "HsScribble"
_PROGRAM_VERSION = "0.0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "Companion generator program for our ECOOP23 submission."

main :: IO ()
main = do
    xs <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null xs then withArgs ["--help"] else id) getOpts
    optionHandler opts

-- Before directly calling your main program, you should warn your user about incorrect arguments, if any.
optionHandler :: MyOptions -> IO ()
optionHandler opts@MyOptions{..} = do
  -- Take the opportunity here to weed out ugly, malformed, or invalid arguments.
  when (null file) $
    putStrLn "Must give a filename" >>
      exitWith (ExitFailure 1)

  unless (null file) $ do
    t <- doesFileExist file
    if not t
      then putStrLn "File does not exist" >> exitWith (ExitFailure 1)
      else execFile opts

execFile :: MyOptions -> IO ()
execFile _opts@MyOptions{..} = do
  putStrLn ("File mode. Input file name: " ++ file)
  when effpi $ putStrLn ("Output Directory: " ++ outdir)
  putStrLn ""
  createDirectoryIfMissing False outdir
  loud <- System.Console.CmdArgs.isLoud
  parseFile file >>= execFile' loud
  where
    execFile' :: Bool -> ErrOr (G ()) -> IO ()
    execFile' _ (Err err) = putStrLn err >> exitWith (ExitFailure 1)
    execFile' loud (Ok g)
      | effpi = effpiGIO (if loud then Loud else Quiet) (takeBaseName file) g
      | otherwise = do
        ppG g
        when (project && loud) $
          putStrLn (intercalate' spacer (map show (projAllRoles g)))
        when (project && not loud) $
          ppRSList (projAllRoles g)
        putStrLn ""
