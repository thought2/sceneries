module PursGen (replaceGens, replaceGenComments, mustache) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Array ((!!))
import Data.Eq ((/=))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe, maybe')
import Data.Monoid (guard)
import Data.Show (class Show, show)
import Data.String (joinWith)
import Data.String.Regex (replace')
import Data.String.Regex.Flags (global, multiline)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile, realpath, writeTextFile)
import Node.Path as Path
import Node.Process (PROCESS, argv)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, id, pure, (#), ($), (<>), (>>=))

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

mustache :: ReplaceDict -> String -> String
mustache dict = replace' reg f
  where
    regexStr = "({{)([a-z][a-zA-Z0-9]*)(}})"

    reg = unsafeRegex regexStr (global <> multiline)

    f = unsafePartial $ \_ [ open, label, close ] ->
      lookup label dict # fromMaybe (open <> label <> close)

replaceGens :: forall eff.
  Array FileReplacement
  -> Eff ( process :: PROCESS, exception :: EXCEPTION, fs :: FS, console :: CONSOLE | eff )
         Unit
replaceGens xs = do
  args <- argv
  dir <- (args !! 2) # maybe' (\_ -> throw $ show ErrWrongArg) pure >>= realpath

  traverse_ (handleFile dir) xs

--------------------------------------------------------------------------------
-- UTIL
--------------------------------------------------------------------------------

replaceGenComments :: ReplaceDict -> String -> String
replaceGenComments dict str = replace' reg f str
  where
    reg = unsafeRegex
      ( "({- GENERATE: ([a-zA-Z][a-zA-Z0-9]*) -})" <>
        "((?:.|\n)*?)" <>
        "({- GENERATE END -})"
      )
      (global <> multiline)

    f = unsafePartial $ \_ [ open, label, content, close ] ->
          open <> maybe content id (lookup label dict) <> close

handleFile :: forall eff.
  String
  -> FileReplacement
  -> Eff ( fs :: FS, exception :: EXCEPTION, console :: CONSOLE | eff ) Unit
handleFile basePath { path, replacements } = do
  content <- readTextFile UTF8 path'
  let content' = replaceGenComments replacements content
  guard (content /= content') $

  log (logStr $ LogPatching path')
  writeTextFile UTF8 (path' <> ".bak") content
  writeTextFile UTF8 path' content'
  where
    path' = Path.concat [ basePath, path ]

logStr :: Log -> String
logStr log = case log of
  LogPatching path -> "Patching " <> path

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

type FileReplacement = { path :: String, replacements :: ReplaceDict }

type ReplaceDict = Map String String

type ReplaceDictEntry = Tuple String String

data Err = ErrWrongArg | ErrRegex String

data Log = LogPatching String

derive instance genericError :: Generic Err _

instance showError :: Show Err where
  show = genericShow

--------------------------------------------------------------------------------
-- DSL
--------------------------------------------------------------------------------

runDsl :: Writer (Array FileReplacement) Unit -> (Array FileReplacement)
runDsl = execWriter

replace :: String -> String -> Array String -> Writer (Array ReplaceDictEntry) Unit
replace x deli ys = tell $ pure (x /\ y)
  where
    y = "\n\n" <> joinWith "\n\n" ys <> "\n\n"

inFile :: String
          -> Writer (Array ReplaceDictEntry) Unit
          -> Writer (Array FileReplacement) Unit
inFile path m = tell $ pure { path, replacements: Map.fromFoldable $ execWriter m }
