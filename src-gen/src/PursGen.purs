module PursGen (replaceGens, replaceGenComments, mustache) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Data.Array (concat, filter, (!!))
import Data.Either (fromRight)
import Data.Eq ((/=))
import Data.Map (Map, lookup)
import Data.Maybe (maybe, maybe')
import Data.Monoid (guard)
import Data.String.Regex (regex, replace')
import Data.String.Regex.Flags (global, multiline)
import Data.Traversable (traverse, traverse_)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (readTextFile, readdir, realpath, stat, writeTextFile)
import Node.Path (FilePath, extname)
import Node.Path (concat) as Path
import Node.Process (PROCESS, argv)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, id, pure, (#), ($), (<#>), (<<<), (<>), (==), (>>=), (>>>))


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

mustache :: Map String String -> String -> String
mustache dict = replace' reg f
  where
    regStr = "({{)([a-z][a-zA-Z0-9]*)(}})"
    reg = unsafePartial $ fromRight $ regex regStr (global <> multiline)
    f = unsafePartial $ \_ [ open, label, close ] ->
      lookup label dict # maybe (open <> label <> close) id

replaceGens :: forall eff.
  Map String String
  -> Eff ( process :: PROCESS, exception :: EXCEPTION, fs :: FS, console :: CONSOLE | eff )
         Unit
replaceGens dict = do
  args <- argv
  dir <- (args !! 2) # maybe' (\_ -> throw errWrongArg) pure >>= realpath
  readDirRec dir
    <#> filter (extname >>> (==) ".purs")
    >>= traverse_ (handleFile dict)
  where
    errWrongArg = "no path argument provided"

--------------------------------------------------------------------------------
-- UTIL
--------------------------------------------------------------------------------

readDirRec :: forall eff.
  FilePath -> Eff ( fs :: FS, exception :: EXCEPTION | eff ) (Array FilePath)
readDirRec path =
  readdir path >>= traverse (f <<< (\x -> Path.concat [path, x])) <#> concat
  where
    f path' = do
      stats <- stat path'
      if isDirectory stats then readDirRec path' else pure [ path' ]

replaceGenComments :: Map String String -> String -> String
replaceGenComments dict str = replace' reg f str
  where
    regStr =
      "({- GENERATE: ([a-zA-Z][a-zA-Z0-9]*) -})" <>
      "((?:.|\n)*?)" <>
      "({- GENERATE END -})"
    reg = unsafePartial $ fromRight $ regex regStr (global <> multiline)
    f = unsafePartial $ \_ [ open, label, content, close ] ->
          open <> maybe content id (lookup label dict) <> close

handleFile :: forall eff.
  Map String String
  -> String
  -> Eff ( fs :: FS, exception :: EXCEPTION, console :: CONSOLE | eff ) Unit
handleFile dict path = do
  content <- readTextFile UTF8 path
  let content' = replaceGenComments dict content
  guard (content /= content') $

  log msg
  writeTextFile UTF8 (path <> ".bak") content
  writeTextFile UTF8 path content'

  where
    msg = "Patching " <> path <> " ."
