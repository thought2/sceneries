module Main where

import Control.Alternative ((<*>))
import Control.Monad.Aff (Aff, launchAff, makeAff, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except (runExcept)
import Data.Array (concat, concatMap, drop, fold, foldM, foldl, foldr, index, length, reverse, snoc, take)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, ForeignError(..), readArray, readInt, readNumber, readString)
import Data.Foreign.Index (readIndex, readProp)
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber)
import Data.Int as Int
import Data.List.NonEmpty (NonEmptyList(..), head)
import Data.Matrix (toArray) as M
import Data.Matrix4 as M
import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.Number as Number
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Data.Vector (toArray)
import Data.Vector3 (Vec3, get3X, get3Y, get3Z, vec3)
import Extensions (LogLevel(..), fail, mapM)
import Graphics.WebGLAll (Attribute, Capacity(..), Mask(..), Mat4, Mode(..), Shaders(..), Uniform, WebGLProg, WebGl, clear, clearColor, drawArr, enable, getCanvasHeight, getCanvasWidth, makeBufferFloat, runWebGL, setUniformFloats, viewport, withShaders)
import Graphics.WebGLAll as Gl
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Partial (crash)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, map, negate, pure, show, (#), ($), (-), (/), (<#>), (<$>), (<<<), (<>), (=<<), (>>=), (>>>))

--------------------------------------------------------------------------------
-- SHADERS
--------------------------------------------------------------------------------

vertexShader :: String
vertexShader =
  """
    attribute vec3 aVertexPosition;

    uniform mat4 uMVMatrix;
    uniform mat4 uPMatrix;

    void main(void) {
      gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
    }
  """

fragmentShader :: String
fragmentShader =
  """
    precision mediump float;

    void main(void) {
      gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
  """

shaders :: Shaders { aVertexPosition :: Attribute Gl.Vec3
                   , uPMatrix :: Uniform Mat4
                   , uMVMatrix:: Uniform Mat4
                   }
shaders = Shaders fragmentShader vertexShader

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

runAff' aff errCb okCb = runAff errCb okCb aff

--main :: forall eff . Eff (console :: CONSOLE | eff) Unit
main = do
  runAff' getData (log <<< show) $ \triangles ->
    runWebGL "glcanvas" log \context -> do

      width <- getCanvasWidth context
      height <- getCanvasHeight context

      withShaders shaders log \bindings -> do
        render width height bindings triangles

--------------------------------------------------------------------------------
-- RENDER
--------------------------------------------------------------------------------

type Bindings a  = { aVertexPosition :: Attribute Gl.Vec3
                  , uPMatrix :: Uniform Mat4
                  , uMVMatrix:: Uniform Mat4
                  , webGLProgram :: WebGLProg
                  | a
                  }


render :: forall eff a
        . Int -> Int -> Bindings a -> Array Triangle -> Eff ( webgl :: WebGl
                                                            , console :: CONSOLE
                                                            | eff
                                                            )
                                                            Unit
render width height bindings triangles =
  do
    clearColor 0.0 0.5 0.0 1.0
    enable DEPTH_TEST

    viewport 0 0 width height
    clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

    let pMatrix =
          M.makePerspective 45.0 (toNumber width / toNumber height) 0.1 100.0

    setUniformFloats bindings.uPMatrix (M.toArray pMatrix)

    let mvMatrix = M.translate (vec3 (-1.5) 0.0 (-10.0)) M.identity

    setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix)

    let xs = triangles
             # concatMap (\(Triangle p1 p2 p3) -> concatMap toArray [p1, p2, p3])

    buf <- makeBufferFloat xs

    drawArr TRIANGLES buf bindings.aVertexPosition

--------------------------------------------------------------------------------
-- DATA
--------------------------------------------------------------------------------

getData :: forall e . Aff (ajax:: AJAX | e) (Array Triangle)
getData = do
  { response } <-
    affjax $ defaultRequest { url = "/data/untitled1.obj", method = Left GET }
  case readWFObj response # parse # runExcept of
    Left err -> fail (show (head err))
    Right xs -> pure xs

parse :: Foreign -> F (Array Triangle)
parse val = do
  verticesLookup <-
    readProp "models" val >>= readArray >>= mapM parseVertices <#> concat
  models <-
    readProp "models" val >>= readArray >>= mapM (parseModel verticesLookup)
  pure $ concat models

  where
    parseVertices val =
      readProp "vertices" val >>= readArray >>= mapM parseVertex

parseModel :: Array Vec3n -> Foreign -> F (Array Triangle)
parseModel verticesLookup val = do
  faces <- readProp "faces" val >>= readArray >>= mapM (parseFace verticesLookup)
  pure $ concat faces

parseVertex :: Foreign -> F Vec3n
parseVertex val = do
  x <- readProp "x" val >>= readNumber
  y <- readProp "y" val >>= readNumber
  z <- readProp "z" val >>= readNumber
  pure $ vec3 x y z

parseFace :: Array Vec3n -> Foreign -> F (Array Triangle)
parseFace verticesLookup val = do
  indices <- readProp "vertices" val >>= readArray >>= mapM
             (\v -> readProp "vertexIndex" v >>= readInt)
  vertices <-
    lookup verticesLookup (map (\x -> x - 1) indices)
    # maybe' (\_ -> fail "lookup error") pure

  do
    a <- index vertices 0
    b <- index vertices 1
    c <- index vertices 2
    d <- index vertices 3

    Just [ Triangle a b c, Triangle a c d ]
    # maybe' (\_d -> fail "triangle error") pure

lookup :: forall a . Array a -> Array Int -> Maybe (Array a)
lookup xs indices =
  let hunde = Tuple xs indices in
  mapM (index xs) indices

partition3 :: forall a b . Int -> (a -> a -> a -> b) -> Array a -> Array b
partition3 offset f xs =
  go [] xs
  where
    go acc xs =
      case take 3 xs of
        [x, y, z] -> go (snoc acc $ f x y z) (drop offset xs)
        _ -> acc

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

foreign import readWFObj :: String -> Foreign

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

data Cube = Cube { triangles :: Array Vec3n }

data Triangle = Triangle Vec3n Vec3n Vec3n

type Vec3n = Vec3 Number
