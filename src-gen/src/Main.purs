module Main where


import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (null, range, replicate)
import Data.Map (fromFoldable)
import Data.Monoid (mempty)
import Data.Semigroup (class Semigroup)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Node.FS (FS)
import Node.Process (PROCESS)
import Prelude (Unit, map, show, (#), ($), (-), (<>), (==), (>>>))
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

gen_show :: String -> String
gen_show name = mustache replacements $ joinWith "\n" $
  [ "instance show{{name}} :: Show {{name}} where"
  , "  show = genericShow"
  ]
  where
    replacements = fromFoldable [ "name" /\ name ]

gen_decode :: String -> String
gen_decode name = mustache replacements $ joinWith "\n" $
  [ "instance decode{{name}} :: Decode {{name}} where"
  , "  decode = genericDecode opts"
  ]
  where
    replacements = fromFoldable [ "name" /\ name ]

gen_deriveGeneric :: String -> String
gen_deriveGeneric name = mustache replacements
  "derive instance generic{{name}} :: Generic {{name}} _"
  where
    replacements = fromFoldable [ "name" /\ name ]

gen_newtype :: String -> String
gen_newtype name = mustache replacements
  "derive instance newtype{{name}} :: Newtype {{name}} _"
  where
    replacements = fromFoldable [ "name" /\ name ]




--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

range' :: Int -> Int -> Array Int
range' n1 n2 =
  if n1 == n2 then [] else range n1 (n2-1)

joinWith' :: String -> Array String -> String
joinWith' x xs =
  if null xs then mempty else joinWith x xs <> " "

nl :: Int -> String
nl n = replicate n "\n" # joinWith mempty

wrap :: forall a. Semigroup a => a -> a -> a
wrap x y = x <> y <> x

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: forall eff.
  Eff ( process :: PROCESS, exception :: EXCEPTION, fs :: FS, console :: CONSOLE | eff )
      Unit
main = replaceGens
  [ { path : "Data/Vec/Extra.purs"
    , replacements : fromFoldable
        [ "Vec"     /\ (range 1 9 # map gen_Vec # format 1)
        , "vec"     /\ (range 1 9 # map gen_vec # format 2)
        , "uncurry" /\ (range 1 9 # map gen_uncurry # format 2)
        , "curry"   /\ (range 1 9 # map gen_curry # format 2)
        ]
    }
  , { path : "WFObjFormat.purs"
    , replacements : fromFoldable
        let xs = [ "Main", "Model", "Vertex", "Face", "VertexRef" ] in
        [ "deriveGeneric" /\ (map gen_deriveGeneric xs # format 2)
        , "newtype" /\ (map gen_newtype xs # format 2)
        , "show" /\ (map gen_show xs # format 2)
        , "decode" /\ (map gen_decode xs # format 2)
        ]
    }
  ]

  where
    format n = joinWith (nl n) >>> wrap (nl 2)
