module Main where

import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (random)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Array (concat, concatMap, drop, filter, foldr, index, length, mapWithIndex, range, snoc, take, zipWith)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Array.NonEmpty as NE
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(Right, Left), either)
import Data.Eq ((/=))
import Data.Foreign (F, Foreign, readArray, readInt, readNumber)
import Data.Foreign.Index (readProp)
import Data.Int (toNumber)
import Data.List.NonEmpty as NEList
import Data.Matrix (toArray) as M
import Data.Matrix4 (identity, makePerspective, translate) as M
import Data.Maybe (Maybe(..), fromJust, maybe, maybe')
import Data.Midi (Event(..), TimedEvent(..))
import Data.Midi.WebMidi (createEventChannel)
import Data.Semigroup.Foldable (fold1)
import Data.String (Pattern(..), split)
import Data.String.NonEmpty (unsafeFromString)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))
import Data.TypeNat (Three, Two)
import Data.Vector (Vec, toArray)
import Data.Vector2 (Vec2, get2X, get2Y, vec2)
import Data.Vector3 (Vec3, vec3)
import Extensions (fail, mapM)
import Graphics.WebGLAll (Attribute, Capacity(DEPTH_TEST), Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT), Mat4, Mode(TRIANGLES), Shaders(Shaders), Uniform, WebGLProg, WebGl, clear, clearColor, drawArr, enable, makeBufferFloat, runWebGL, setUniformFloats, viewport, withShaders)
import Graphics.WebGLAll as Gl
import Math (pi, sin, (%))
import Network.HTTP.Affjax (AJAX, get)
import Partial.Unsafe (unsafePartial)
import Pathy (class IsDirOrFile, class IsRelOrAbs, AbsDir, AbsFile, RelDir, RelFile, SandboxedPath, dir, extension, file, fileName, parseRelFile, posixParser, posixPrinter, printPath, rootDir, sandboxAny, unsafePrintPath, (</>))
import Prelude (class Semigroup, Unit, bind, const, discard, flip, id, map, max, negate, pure, show, sub, (#), ($), (*), (+), (-), (/), (<), (<#>), (<$>), (<*>), (<<<), (<>), (==), (>>=), (>>>))
import Signal (Signal, filterMap, foldp, map2, merge, mergeMany, runSignal, (~>))
import Signal.Channel (CHANNEL, subscribe)
import Signal.DOM (DimensionPair, CoordinatePair, animationFrame, keyPressed, mousePos, windowDimensions)
import Signal.Time (Time)

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

-- main :: forall a.
--   Eff
--     ( ajax :: AJAX
--     , console :: CONSOLE
--     , clock :: CLOCK
--     , random :: RANDOM
--     | a
--     )
--     (Canceler
--        ( ajax :: AJAX
--        , console :: CONSOLE
--        , clock :: CLOCK
--        , random :: RANDOM
--        | a
--        )
--     )
main = do
  runAff' getScenes (log <<< show) $ \scenes ->
    runWebGL "glcanvas" log \context -> do
      withShaders shaders log \bindings -> do
        config <- mkConfig scenes

        renderInit config
        mainFrp config bindings

--------------------------------------------------------------------------------
-- FRP
--------------------------------------------------------------------------------

mainFrp config bindings = do
  sigTime <- animationFrame <#> map SigTime
  sigSize <- windowDimensions <#> map SigSize
  sigInput1 <- keyboardMouseInput <#> map SigInput
  sigInput2 <- midiInput <#> map SigInput

  let sigState = merge4 sigTime sigSize sigInput1 sigInput2
                 # foldp update initState

  runSignal (sigState ~> (\state -> render config state bindings))

keysPressed :: forall eff. Eff ( dom :: DOM | eff) (Signal Key)
keysPressed =
  mapM f keys <#> mergeMany >>> (unsafePartial fromJust)
  where
    f key = keyPressed (keyToInt key) <#> map (const key)

    keys :: Array Key
    keys = rangeChar '0' '9' # map CharKey

rangeChar :: Char -> Char -> Array Char
rangeChar c1 c2 = range (toCharCode c1) (toCharCode c2) # map fromCharCode

merge3 :: forall a. Signal a -> Signal a -> Signal a -> Signal a
merge3 s1 s2 s3 = merge s1 s2 # merge s3

merge4 :: forall a. Signal a -> Signal a -> Signal a -> Signal a -> Signal a
merge4 s1 s2 s3 s4 = merge3 s1 s2 s3 # merge s4

keyboardMouseInput :: forall eff. Eff ( dom :: DOM | eff) (Signal Input)
keyboardMouseInput = do
  keys <- keysPressed <#> map SigKey
  mouse <- mousePos <#> map SigMouse
  sizes <- windowDimensions

  merge keys mouse
    # foldp update init
    # filterMap
      (\{ lastKey : CharKey c, value } -> Input <$> pure c <*> value)
      (Input '0' 0.0)
    # map2 (\{ w } (Input c v) -> Input c (v / toNumber w)) sizes
    # pure

  where
    init = { lastKey : CharKey '0', value : Nothing }
    update x m = case x /\ m of
      SigKey lastKey /\ _ -> m { lastKey = lastKey, value = Nothing }
      SigMouse { x } /\ { lastKey : CharKey '1' } -> m { value = Just $ toNumber x }
      SigMouse { x } /\ { lastKey : CharKey '2' } -> m { value = Just $ toNumber x }
      _ -> m

midiInput :: forall eff. Eff ( channel :: CHANNEL | eff) (Signal Input)
midiInput = do
  sigMidi <- createEventChannel <#> subscribe
  sigMidi
    # filterMap f (Input '0' 0.5)
    # pure
  where
    f (TimedEvent { event : (Just (ControlChange _ _ n)) }) = Just $ Input '2' (reMap' n)
    f _ = Nothing
    reMap' n = reMap (vec2 0.0 127.0) spacePos (toNumber n)

keyToInt :: Key -> Int
keyToInt key = case key of
  CharKey char -> toCharCode char

intToKey :: Int -> Key
intToKey n = CharKey (fromCharCode n)

--------------------------------------------------------------------------------
-- CONFIG
--------------------------------------------------------------------------------

--mkConfig :: NonEmptyArray Scene -> Config
mkConfig scenes = do
  randomField <- range 0 n # mapM (const randVec)
  pure $ Config
    { randomField
    , scenes
    }
  where
    n = map (\(Scene s) -> length s) scenes # foldr max 0
    randVec =
      randomVec2n
        <#> map (reMap spacePos spaceNegPos)
        >>> (\v2 -> vec2to3 v2 18.0)

randomVec2n :: Eff _ Vec2n
randomVec2n =
  vec2 <$> random <*> random

randomVec3n :: Eff _ Vec3n
randomVec3n =
  vec3 <$> random <*> random <*> random

vec2to3 :: forall a . Vec2 a -> a -> Vec3 a
vec2to3 vec z =
  vec3 (get2X vec) (get2Y vec) z

--------------------------------------------------------------------------------
-- MORPH CHAIN
--------------------------------------------------------------------------------

instance semigroupScaleFn :: Semigroup (ScaleFn a) where
  append (ScaleFn t1 f1) (ScaleFn t2 f2) = ScaleFn (t1 + t2) f
    where
      f t = if t < t1 then f1 t else f2 (t - t1)

toFunction :: forall a. ScaleFn a -> Time -> a
toFunction (ScaleFn _ f) = f

morph' :: forall a . Morph a => a -> a -> Number -> a
morph' x y t = morph t x y

--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------

initState :: State
initState =
  State
    { pcts : { "n1" : 0.0, "n2" : 0.0 }
    , time : 0.0
    , size : vec2 0 0
    }

update :: Sig -> State -> State
update sig state@(State st) =
  case sig of
    SigTime t ->
      State $ st { time = t }

    SigSize { w, h } ->
      State $ st { size = vec2 w h }

    SigInput (Input '1' pct) -> State $ st { pcts {"n1" = pct} }
    SigInput (Input '2' pct) -> State $ st { pcts {"n2" = pct} }

    _ -> state

--------------------------------------------------------------------------------
-- SELECTORS
--------------------------------------------------------------------------------

selectScaleFn :: Config -> State -> ScaleFn Scene
selectScaleFn (Config { scenes, randomField }) state =
  NE.concatMap f scenes
    # fold1
  where
    f scene =
      NE.singleton (ScaleFn 1.0 (morph' randomScene scene)) <>
      NE.singleton (ScaleFn 1.0 (morph' scene randomScene))

    n = NE.length scenes * 2
    randomScene = Scene $ map (\v -> Triangle v v v) randomField

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
        . Config -> State -> Bindings a -> Eff ( webgl :: WebGl, console :: CONSOLE | eff) Unit
render config@(Config { scenes, randomField }) state@(State { pcts, time, size }) bindings =
  do
    clear [ COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT ]
    setUniformFloats bindings.uPMatrix (M.toArray pMatrix)
    setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix)

    let (Tuple w h) = get2 size
    viewport 0 0 w h

    log $ show [pcts.n1, pcts.n2]

    buf <- makeBufferFloat xs

    drawArr TRIANGLES buf bindings.aVertexPosition

    where

      ScaleFn dur scaleFn = selectScaleFn config state

      velocity = pcts.n1

      t = sin ((time / loopTime) * two * pi)
          # reMap spaceNegPos spacePos

      t' = pcts.n2

      t'' = (time / loopTime) % 1.0

      xs = do
        let (Scene tris) = scaleFn (t'' * dur)
        (Triangle p1 p2 p3) <- tris
        concatMap toArray [ p1, p2, p3 ]

      loopTime = 20.0 * 1000.0

      getPct offsetPct =
        sin ((time / loopTime * two * pi) + (offsetPct * loopTime))
          # reMap spacePos spaceNegPos

      Tuple width height = get2 size

      mvMatrix =
        M.translate (vec3 zero 0.0 (-20.0)) M.identity

      pMatrix =
        M.makePerspective 45.0 (toNumber width / toNumber height) 0.1 100.0

mapWithPct :: forall a b . (Number -> a -> b) -> Array a -> Array b
mapWithPct f xs =
  mapWithIndex (\i x -> f (toNumber i / n) x) xs
  where
    n = toNumber (length xs)

renderInit :: forall eff . Config -> Eff ( webgl :: WebGl | eff ) Unit
renderInit _ = do
  clearColor 0.5 0.5 0.5 1.0
  enable DEPTH_TEST

get2 :: forall a . Vec2 a -> Tuple a a
get2 vec = Tuple (get2X vec) (get2Y vec)

--------------------------------------------------------------------------------
-- DATA
--------------------------------------------------------------------------------

getScenes :: forall e . Aff (ajax:: AJAX | e) (NonEmptyArray Scene)
getScenes = do
  paths <- getIndex indexDir <#> filter predFn
  mapM getScene paths >>= fromArray >>> maybe' (\_ -> fail errMsgEmpty) pure
  where
    predFn = fileName >>> extension >>> map ((==) objExt) >>> maybe false id
    objExt = unsafePartial $ unsafeFromString "obj"
    indexDir = dir (SProxy :: SProxy "data")
    errMsgEmpty = "no scenes"

getIndex :: forall e . RelDir -> Aff (ajax:: AJAX | e) (Array RelFile)
getIndex folder =
  get (printPath' (sandboxAny path))
    >>= (_.response >>> parseIndexFile >>> either fail pure)
    <#> (map ((</>) folder))
  where
    path = folder </> indexFile
    indexFile = file (SProxy :: SProxy "index.txt")

printPath' :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath a b -> String
printPath' path = unsafePrintPath posixPrinter path

parseIndexFile :: String -> Either String (Array RelFile)
parseIndexFile str =
  split (Pattern "\n") str
    # filter ((/=) "")
    # mapM (parseRelFile posixParser)
    # maybe (Left errMsg) Right
  where
    errMsg = "Invalid index file"

getScene :: forall e . RelFile -> Aff (ajax:: AJAX | e) Scene
getScene path = do
  { response } <- get (printPath' (sandboxAny path))
  case readWFObj response # parse # runExcept of
    Left err -> fail (show (NEList.head err))
    Right xs -> pure $ Scene xs

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

--------------------------------------------------------------------------------
-- SHORTHANDS
--------------------------------------------------------------------------------

zero :: Number
zero = 0.0

onePos :: Number
onePos = 1.0

one :: Number
one = onePos

two :: Number
two = 2.0

oneNeg :: Number
oneNeg = -1.0

spaceNegPos :: Vec Two Number
spaceNegPos = vec2 oneNeg onePos

spacePos :: Vec Two Number
spacePos = vec2 zero onePos

--------------------------------------------------------------------------------
-- UTIL
--------------------------------------------------------------------------------

reMap :: Pair Number -> Pair Number -> Number -> Number
reMap pair1 pair2 value =
  pivot2 + (pct * dist2)
  where
    pivot1 = get2X pair1
    pivot2 = get2X pair2
    dist1 = sub' (get2 pair1)
    dist2 = sub' (get2 pair2)
    sub' = uncurry (flip sub)
    pct = (value - pivot1) / dist1

section :: forall a b . Int -> Int -> Array a -> Array (Array a)
section stride offset xs =
  go [] xs
  where
    go acc xs =
      let ys = take stride xs in
      if length ys == stride then
        go (snoc acc ys) (drop offset xs)
      else
        acc

section2 :: forall a. Int -> Array a -> Array (Tuple a a)
section2 offset xs =
  section 2 offset xs
  # map (unsafePartial (\[a, b] -> a /\ b))

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

foreign import readWFObj :: String -> Foreign

--------------------------------------------------------------------------------
-- CLASS
--------------------------------------------------------------------------------

class Morph a where
  morph :: Number -> a -> a -> a

instance morphNumber :: Morph Number where
  morph pct x y = x + (pct * (y - x))

instance morphVec3 :: Morph a => Morph (Vec Three a) where
  morph pct x y =
    morph pct <$> x <*> y

instance morphTriangle :: Morph Triangle where
  morph pct (Triangle x1 y1 z1) (Triangle x2 y2 z2) =
    Triangle (f x1 x2) (f y1 y2) (f z1 z2)
    where
      f = morph pct

instance morphArray :: Morph a => Morph (Array a) where
  morph pct xs ys =
    zipWith (morph pct) xs ys

instance morphScene :: Morph Scene where
  morph pct (Scene s1) (Scene s2) = Scene $ morph pct s1 s2

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

newtype Config = Config
  { randomField :: Array Vec3n
  , scenes :: NonEmptyArray Scene
  }

newtype State = State
  { pcts :: { "n1" :: Number, "n2" :: Number }
  , time :: Number
  , size :: Vec2i
  }

newtype Scene = Scene (Array Triangle)

data Triangle = Triangle Vec3n Vec3n Vec3n

type Vec3n = Vec3 Number
type Vec2n = Vec2 Number
type Vec2i = Vec2 Int

type Pair a = Vec2 a

data Sig
  = SigTime Number
  | SigSize DimensionPair
  | SigKey Key
  | SigMouse CoordinatePair
  | SigMidi
  | SigInput Input

data Input = Input Char Number

data ScaleFn a = ScaleFn Time (Time -> a)

data Key = CharKey Char
