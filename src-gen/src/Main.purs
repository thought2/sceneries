module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (concat, mapWithIndex, null, range, replicate, (:))
import Data.Foldable (foldr, surround)
import Data.Map (Map, fromFoldable)
import Data.Monoid (mempty)
import Data.Semigroup (class Semigroup)
import Data.StrMap (insert)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Node.FS (FS)
import Node.Process (PROCESS)
import Prelude (Unit, map, show, (#), ($), (-), (<>), (==))
import PursGen (mustache, replaceGens)

--------------------------------------------------------------------------------
-- Generated
--------------------------------------------------------------------------------

gen_uncurry :: Int -> String
gen_uncurry i = mustache replacements $ joinWith (nl 1) $
  [ "uncurry{{i}} :: forall r a. ({{as}}r) -> Vec D{{i}} a -> r"
  , "uncurry{{i}} f v = f{{args}}"
  ]
  where
    replacements = fromFoldable
      [ "i"    /\ show i
      , "as"   /\ joinWith "" (replicate i "a -> ")
      , "args" /\ (range' 0 i # map (\i' -> " (v !! d" <> show i' <> ")") # joinWith mempty)
      ]

gen_curry :: Int -> String
gen_curry i = mustache replacements $ joinWith (nl 1) $
  [ "curry{{i}} :: forall r a. (Vec D{{i}} a -> r) -> {{as}}r"
  , "curry{{i}} f{{xs}} = f $ vec{{i}}{{xs}}"
  ]
  where
    replacements = fromFoldable
      [ "i"    /\ show i
      , "as"   /\ joinWith "" (replicate i "a -> ")
      , "xs" /\ (range' 0 i # map (\i' -> " x" <> show i') # joinWith mempty)
      ]

gen_vec :: Int -> String
gen_vec i = mustache replacements $ joinWith "\n" $
  [ "vec{{i}} :: forall a. {{as}}Vec D{{d}} a"
  , "vec{{i}} {{xs1}}= {{xs2}}empty"
  ]
  where
    replacements = fromFoldable
      [ "i"   /\ show i
      , "as"  /\ joinWith "" (replicate i "a -> ")
      , "d"   /\ show i
      , "xs1" /\ (range' 0 i # map (\i' -> "x" <> show i' <> " ") # joinWith mempty)
      , "xs2"  /\ (range' 0 i # map (\i' -> "x" <> show i' <> " +> ") # joinWith mempty)
      ]

-- genGet :: Int -> String -> String
-- genGet i s = mustache replacements $ joinWith "\n" $
--   [ "get{{s}} :: forall s a. Nat s => Lt D{{i}} s => Vec s a -> a"
--   , "get{{s}} = get d{{i}}"
--   ]
--   where
--     replacements = fromFoldable [ "i" /\ show i, "s" /\ s ]

-- genSet :: Int -> String -> String
-- genSet i s = mustache replacements $ joinWith "\n" $
--   [ "set{{s}} :: forall s a."
--   , "  Nat s => Lt D{{i}} s => a -> Vec s a -> Vec s a"
--   , "set{{s}} = set' d{{i}}"
--   ]
--   where
--     replacements = fromFoldable [ "i" /\ show i, "s" /\ s ]

-- genOver :: Int -> String -> String
-- genOver i s = mustache replacements $ joinWith "\n" $
--   [ "over{{s}} :: forall s a."
--   , "  Nat s => Lt D{{i}} s => (a -> a) -> Vec s a -> Vec s a"
--   , "over{{s}} = over d{{i}}"
--   ]
--   where
--     replacements = fromFoldable [ "i" /\ show i, "s" /\ s ]

gen_Vec :: Int -> String
gen_Vec i = mustache replacements
  "type Vec{{i}} = Vec D{{i}}"
  where
    replacements = fromFoldable [ "i" /\ show i ]

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

range' :: Int -> Int -> Array Int
range' n1 n2 =
  if n1 == n2 then [] else range n1 (n2-1)

joinWith' :: String -> Array String -> String
joinWith' x xs =
  if null xs then mempty else joinWith x xs <> " "

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

nl :: Int -> String
nl n = replicate n "\n" # joinWith mempty

wrap :: forall a. Semigroup a => a -> a -> a
wrap x y = x <> y <> x

main :: forall eff.
  Eff ( process :: PROCESS, exception :: EXCEPTION, fs :: FS, console :: CONSOLE | eff )
      Unit
main =
  [ "Vec" /\ (range 1 9 # map gen_Vec # joinWith (nl 1) # wrap (nl 2))
  , "vec" /\ (range 1 9 # map gen_vec # joinWith (nl 2) # wrap (nl 2))
  , "uncurry" /\ (range 1 9 # map gen_uncurry # joinWith (nl 2) # wrap (nl 2))
  , "curry" /\ (range 1 9 # map gen_curry # joinWith (nl 2) # wrap (nl 2))
  ]
  # fromFoldable
  # replaceGens
