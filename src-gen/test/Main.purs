module Test.Main where

import Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import PursGen (replaceGenComments)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall eff.
  Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    | eff
    )
    Unit
main = runTest do
  test "replaceGenComments" do
    let dict = Map.fromFoldable [ "a" /\ "bbb" ]
    assert "" $ (replaceGenComments dict "abc") == "abc"

    let str1 = "abc {- GENERATE a -}aa\na{- GENERATE END -} def"
        str2 = "abc {- GENERATE a -}bbb{- GENERATE END -} def"

    log (replaceGenComments dict str1)

    assert "" $ (replaceGenComments dict str1) == str2
