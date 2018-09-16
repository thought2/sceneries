module Main where

import Color (Color, graytone, toRGBA')
import Control.Monad.Aff (Aff, Canceler(..), launchAff, liftEff', makeAff, makeAff', runAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, message)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import Control.Parallel (parallel)
import DOM (DOM)
import Data.Array (concat, concatMap, drop, filter, foldr, index, insertAt, length, mapWithIndex, range, snoc, take, unsafeIndex, zip, zipWith)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Array.NonEmpty as NE
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(Right, Left), either)
import Data.Enum (class Enum, succ)
import Data.Eq (class Eq, (/=))
import Data.Foreign (F, Foreign, readArray, readInt, readNumber, toForeign)
import Data.Foreign.Index (readProp)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Enum (genericPred, genericSucc)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Monoid (genericMempty)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Semigroup (genericAppend)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.List.NonEmpty as NEList
import Data.Matrix (toArray) as M
import Data.Matrix4 (identity, makePerspective, translate) as M
import Data.Maybe (Maybe(..), fromJust, maybe, maybe')
import Data.Midi (Event(..), TimedEvent(..))
import Data.Midi.WebMidi (createEventChannel)
import Data.Monoid (class Monoid, mempty)
import Data.Record.Builder (build)
import Data.Record.Builder as RB
import Data.Semigroup.Foldable (fold1, foldMap1)
import Data.String (Pattern(..), split)
import Data.String.NonEmpty (unsafeFromString)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))
import Data.TypeNat (class Sized, Three, Two, sized)
import Data.Typelevel.Num (class Lt, class Nat, class Pos, class Trich, D0, D1, D2, D3, D4, D9, d0, d1, d2, d3, d4, d5, d6, reifyInt, toInt)
import Data.Typelevel.Undefined (undefined)
import Data.Vector (Vec(..), fill, toArray)
import Data.Vector2 (Vec2, get2X, get2Y, vec2)
import Data.Vector3 (Vec3, vec3)
import Debug.Trace (spy)
import Extensions (fail, mapM)
import Graphics.WebGLAll (Attribute, Capacity(DEPTH_TEST), EffWebGL, Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT), Mat4, Mode(TRIANGLES), Shaders(Shaders), Uniform, WebGLContext, WebGLProg, WebGl, clear, clearColor, drawArr, enable, getCanvasHeight, getCanvasWidth, makeBufferFloat, runWebGL, setUniformFloats, viewport, withShaders)
import Graphics.WebGLAll as Gl
import Math (cos, pi, sin, (%))
import Network.HTTP.Affjax (AJAX, get)
import Partial.Unsafe (unsafePartial)
import Pathy (class IsDirOrFile, class IsRelOrAbs, AbsDir, AbsFile, Dir, Path, Rel, RelDir, RelFile, SandboxedPath, dir, extension, file, fileName, parseRelFile, posixParser, posixPrinter, printPath, rootDir, sandboxAny, unsafePrintPath, (</>))
import Prelude (class Functor, class Ord, class Semigroup, class Show, Unit, bind, compare, const, discard, flip, id, map, max, negate, pure, show, sub, unit, (#), ($), (*), (+), (-), (/), (<), (<#>), (<$>), (<*>), (<<<), (<>), (==), (>>=), (>>>))
import Signal (Signal, filterMap, foldp, map2, merge, mergeMany, runSignal, (~>))
import Signal.Channel (CHANNEL, subscribe)
import Signal.DOM (DimensionPair, CoordinatePair, animationFrame, keyPressed, mousePos, windowDimensions)
import Signal.Time (Time)
import Type.Prelude (Proxy(..))

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

main :: forall eff.
  Eff
    ( exception :: EXCEPTION
    , webgl :: WebGl
    , ajax :: AJAX
    , channel :: CHANNEL
    , console :: CONSOLE
    , dom :: DOM
    , random :: RANDOM
    , timer :: TIMER
    | eff
    )
    (Canceler
       ( webgl :: WebGl
       , ajax :: AJAX
       , channel :: CHANNEL
       , console :: CONSOLE
       , dom :: DOM
       , random :: RANDOM
       , timer :: TIMER
       | eff
       )
    )
main = launchAff $ do

  let (StaticConfig {canvasId}) = staticConfig

  context <- runWebGLAff canvasId

  bindings <- withShadersAff shaders

  dynConfig <- getDynConfig context

  let config = f staticConfig dynConfig
      initState' = initState config

  liftEff $ do
    renderInit config initState'
    mainFrp config

--------------------------------------------------------------------------------
-- AFF-IFY
--------------------------------------------------------------------------------

withShadersAff :: forall eff a.
  Shaders { | a }
  -> Aff ( webgl ∷ WebGl | eff ) { webGLProgram ∷ WebGLProg | a }
withShadersAff arg = makeAff (\err ok -> withShaders arg (error >>> err) ok)

runWebGLAff :: forall eff.
  String
  -> Aff ( webgl ∷ WebGl | eff ) WebGLContext
runWebGLAff arg =
  makeAff f
  where
    f errCb okCb = runWebGL arg (error >>> errCb) (unsafeCoerceEff <<< okCb)

--------------------------------------------------------------------------------
-- FRP
--------------------------------------------------------------------------------

mainFrp config = do
  sigTime <- animationFrame <#> map SigTime
  sigSize <- windowDimensions <#> map SigSize
  sigInput1 <- keyboardMouseInput <#> map SigInput
  sigInput2 <- midiInput <#> map SigInput

  let signals = merge4 sigTime sigSize sigInput1 sigInput2
      sigState = foldp update (initState config) signals

  _ <- debug config signals sigState
  runSignal (sigState ~> (\state -> render config state))

keysPressed :: forall eff. Eff ( dom :: DOM | eff) (Signal Key)
keysPressed =
  mapM f keys <#> mergeMany >>> (unsafePartial fromJust)
  where
    f key = keyPressed (keyToInt key) <#> map (const key)

    keys :: Array Key
    keys = rangeChar '0' '9' # map CharKey

rangeChar :: Char -> Char -> Array Char
rangeChar c1 c2 = range (toCharCode c1) (toCharCode c2) # map fromCharCode

keyboardMouseInput :: forall eff. Eff ( dom :: DOM | eff) (Signal Input)
keyboardMouseInput = do
  keys <- keysPressed <#> map SigKey
  mouse <- mousePos <#> map SigMouse
  sizes <- windowDimensions

  merge keys mouse
    # foldp update init
    # filterMap
      (\{ lastKey : CharKey c, value } -> Input <$> pure 0 <*> value)
      (Input 0 0.0)
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
    # filterMap f (Input 0 0.5)
    # pure
  where
    f (TimedEvent { event : (Just (ControlChange _ n pct)) }) =
      case n of
        14 -> Just $ Input 0 (reMap' pct)
        15 -> Just $ Input 1 (reMap' pct)
        16 -> Just $ Input 2 (reMap' pct)
        17 -> Just $ Input 3 (reMap' pct)
        18 -> Just $ Input 4 (reMap' pct)
        19 -> Just $ Input 5 (reMap' pct)
        20 -> Just $ Input 6 (reMap' pct)
        21 -> Just $ Input 7 (reMap' pct)
        22 -> Just $ Input 8 (reMap' pct)
        _ -> Nothing
    f _ = Nothing
    reMap' n = reMap (vec2 0.0 127.0) spacePos (toNumber n)

merge3 :: forall a. Signal a -> Signal a -> Signal a -> Signal a
merge3 s1 s2 s3 = merge s1 s2 # merge s3

merge4 :: forall a. Signal a -> Signal a -> Signal a -> Signal a -> Signal a
merge4 s1 s2 s3 s4 = merge3 s1 s2 s3 # merge s4

keyToInt :: Key -> Int
keyToInt key = case key of
  CharKey char -> toCharCode char

intToKey :: Int -> Key
intToKey n = CharKey (fromCharCode n)

--------------------------------------------------------------------------------
-- CONFIG
--------------------------------------------------------------------------------

staticConfig :: StaticConfig
staticConfig = StaticConfig
  { canvasId : "glcanvas"
  , backgroundColor : graytone 0.5
  , dataDir : dir (SProxy :: SProxy "data")
  }

staticConfig' =
  { canvasId : "glcanvas"
  , dataDir : dir (SProxy :: SProxy "data")
  , backgroundColor : graytone 0.5
  , fieldZ : 18.0
  , zNear : 0.1
  , zFar : 100.0
  }


getDynConfig :: forall eff.
  WebGLContext
  -> Aff ( ajax :: AJAX, webgl :: WebGl, random :: RANDOM | eff ) DynConfig
getDynConfig canvasContext = do
  scenes <- getScenes
  bindings <- withShadersAff shaders
  liftEff $ do
    let n = map (\(Scene s) -> length s) scenes # foldr max 0
    size <- getCanvasSize canvasContext
    randomField <- range 0 n # mapM (const randVec)
    pure $ DynConfig
      { randomField
      , scenes
      , size
      , bindings
      }
    where
      { fieldZ } = staticConfig'
      randVec =
        randomVec2n
          <#> map (reMap spacePos spaceNegPos)
          >>> (\v2 -> vec2to3 v2 fieldZ)


-- mkDynConfig scenes = do
--   randomField <- range 0 n # mapM (const randVec)
--   pure $ DynConfig
--     { randomField
--     , scenes
--     }
--   where
--     n = map (\(Scene s) -> length s) scenes # foldr max 0
--     randVec =
--       randomVec2n
--         <#> map (reMap spacePos spaceNegPos)
--         >>> (\v2 -> vec2to3 v2 18.0)

getCanvasSize :: forall eff. WebGLContext -> Eff ( webgl :: WebGl | eff ) Vec2i
getCanvasSize ctx =
  vec2 <$> getCanvasWidth ctx <*> getCanvasHeight ctx

randomVec2n :: forall eff. EffRandom eff Vec2n
randomVec2n =
  vec2 <$> random <*> random

randomVec3n :: forall eff. EffRandom eff Vec3n
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

instance functorScaleFn :: Functor ScaleFn where
  map f (ScaleFn t f') = ScaleFn t (f' >>> f)

toFunction :: forall a. ScaleFn a -> Time -> a
toFunction (ScaleFn _ f) = f

morph' :: forall a . Morph a => a -> a -> Number -> a
morph' x y t = morph t x y

--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------

initState :: Config -> State
initState (Config { size }) =
  State
    { pcts : vec9' 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
    , time : 0.0
    , size
    }

update :: Sig -> State -> State
update sig state@(State st) =
  case sig of
    SigTime t ->
      State $ st { time = t }

    SigSize { w, h } ->
      State $ st { size = vec2 w h }

    SigInput (Input 0 pct) -> State $ st { pcts = set' d0 pct st.pcts }
--    SigInput (Input IdxNine1 pct) -> State $ st { pcts = setNine IdxNine1 pct st.pcts }

    _ -> state

--------------------------------------------------------------------------------
-- SELECTORS
--------------------------------------------------------------------------------

selectScaleFn :: Config -> State -> ScaleFn Scene
selectScaleFn (Config { scenes, randomField }) state =
  foldMap1 f scenes
  where
    f scene =
      ScaleFn 1.0 (cos' >>> morph' randomScene scene) <>
      ScaleFn 1.0 (cos' >>> morph' scene randomScene)

--    t = 1.0 / toNumber (NE.length scenes * 2)
    cos' = reMap (vec2 0.0 1.0) (vec2 pi (2.0 * pi)) >>> cos >>> reMap spaceNegPos spacePos
    randomScene = Scene $ map (\v -> Triangle v v v) randomField

--------------------------------------------------------------------------------
-- DEBUG
--------------------------------------------------------------------------------

debug config signals sigState = do
  runSignal (signals ~> f)
  runSignal (sigState ~> g)
  where
    f (SigInput (Input a b)) = let _ = spy b in pure unit
    f _ = pure unit
    g (State {pcts}) = let _ = spy pcts in pure unit
    g _ = pure unit


--------------------------------------------------------------------------------
-- RENDER
--------------------------------------------------------------------------------

renderInit :: RenderFn
renderInit (Config { backgroundColor }) _ = do
  enable DEPTH_TEST

render :: RenderFn
render config state =
  do
    clear [ COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT ]
    renderBackground config state
    renderPerspective config state
    renderScene config state

renderScene :: RenderFn
renderScene config @ (Config { bindings }) state @ (State { pcts, time }) = do
    setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix)

    buf <- makeBufferFloat xs

    drawArr TRIANGLES buf bindings.aVertexPosition

    where

      ScaleFn dur scaleFn = selectScaleFn config state

      velocity = get' d0 pcts

      t = sin ((time / maxLoopTime) * two * pi)
          # reMap spaceNegPos spacePos

      t' = get' d1 pcts

      t'' = (time / ((get' d2 pcts) * maxLoopTime)) % 1.0

      xs = do
        let (Scene tris) = scaleFn (t'' * dur)
        (Triangle p1 p2 p3) <- tris
        concatMap toArray [ p1, p2, p3 ]

      maxLoopTime = 1000.0 * 1000.0

      getPct offsetPct =
        sin ((time / maxLoopTime * two * pi) + (offsetPct * maxLoopTime))
          # reMap spacePos spaceNegPos

      mvMatrix =
        M.translate (vec3 zero 0.0 (-20.0)) M.identity


renderPerspective :: RenderFn
renderPerspective (Config { bindings }) (State { size, pcts }) = do
  viewport 0 0 width height
  setUniformFloats bindings.uPMatrix (M.toArray pMatrix)
  where
    width /\ height = vec2_getTuple size
    { zNear, zFar } = staticConfig'
    aspect = uncurry (/) (map toNumber size # vec2_getTuple)
    pMatrix = M.makePerspective
              (reMap (vec2 0.0 1.0) (vec2 0.0 360.0) (get' d4 pcts))
              (reMap (vec2 0.0 1.0) (vec2 0.0 5.0) (get' d5 pcts))
              zNear
              zFar

renderBackground :: RenderFn
renderBackground _ (State { pcts }) =
  let { r, g, b, a } = toRGBA' (graytone (get' d6 pcts)) in
  clearColor r g b a

type RenderFn = forall eff a. Config -> State -> EffWebGL eff Unit
type RenderFn1 = forall eff a. Config -> State -> a -> EffWebGL eff Unit

vec2_getTuple :: forall a . Vec2 a -> Tuple a a
vec2_getTuple vec = Tuple (get2X vec) (get2Y vec)

--------------------------------------------------------------------------------
-- DATA
--------------------------------------------------------------------------------

getScenes :: forall e . Aff (ajax:: AJAX | e) (NonEmptyArray Scene)
getScenes = do
  paths <- getIndex dataDir <#> filter predFn
  mapM getScene paths >>= fromArray >>> maybe' (\_ -> fail errMsgEmpty) pure
  where
    predFn = fileName >>> extension >>> map ((==) objExt) >>> maybe false id
    objExt = unsafePartial $ unsafeFromString "obj"
    { dataDir } = staticConfig'
    errMsgEmpty = "no scenes"

getIndex :: forall e . RelDir -> Aff (ajax:: AJAX | e) (Array RelFile)
getIndex folder =
  get (printPath' (sandboxAny path))
    >>= (_.response >>> parseIndexFile >>> either fail pure)
    <#> (map ((</>) folder))
  where
    path = folder </> indexFile
    indexFile = file (SProxy :: SProxy "index.txt")

printPath' :: forall a b.
  IsRelOrAbs a => IsDirOrFile b => SandboxedPath a b -> String
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
  models <- readProp "models" val >>= readArray
  verticesLookup <-
    mapM parseVertices models <#> concat
  models' <-
    mapM (parseModel verticesLookup) models <#> concat
  pure models'

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

spaceCircle :: Vec Two Number
spaceCircle = vec2 zero (2.0 * pi)

halfPi = pi / 2.0

--------------------------------------------------------------------------------
-- UTIL
--------------------------------------------------------------------------------

reMap :: Pair Number -> Pair Number -> Number -> Number
reMap pair1 pair2 value =
  pivot2 + (pct * dist2)
  where
    pivot1 = get2X pair1
    pivot2 = get2X pair2
    dist1 = sub' (vec2_getTuple pair1)
    dist2 = sub' (vec2_getTuple pair2)
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

mapWithPct :: forall a b . (Number -> a -> b) -> Array a -> Array b
mapWithPct f xs =
  mapWithIndex (\i x -> f (toNumber i / n) x) xs
  where
    n = toNumber (length xs)

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

type StaticConfigRow a =
  ( canvasId :: String
  , backgroundColor :: Color
  , dataDir :: Path Rel Dir
  | a
  )

newtype StaticConfig = StaticConfig (Record (StaticConfigRow ()))

type DynConfigRow a =
  ( randomField :: Array Vec3n
  , scenes :: NonEmptyArray Scene
  , size :: Vec2i
  , bindings :: Bindings
  | a
  )

newtype DynConfig = DynConfig (Record (DynConfigRow ()))

newtype Config = Config (Record (DynConfigRow (StaticConfigRow ())))

f :: StaticConfig -> DynConfig -> Config
f (StaticConfig x) (DynConfig y) =
  Config $ mergeRecords x y

mergeRecords :: forall r1 r2 r3. Union r2 r3 r1 => { | r3 } -> { | r2 } -> { | r1 }
mergeRecords r1 r2 = build (RB.merge r1) r2

newtype State = State
  { pcts :: Vec' D9 Number
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

data Input = Input Int Number

data ScaleFn a = ScaleFn Time (Time -> a)

data Key = CharKey Char

type Bindings =
  { aVertexPosition :: Attribute Gl.Vec3
  , uPMatrix :: Uniform Mat4
  , uMVMatrix:: Uniform Mat4
  , webGLProgram :: WebGLProg
  }

type EffRandom eff a = Eff ( random :: RANDOM | eff ) a

--------------------------------------------------------------------------------
-- Vec
--------------------------------------------------------------------------------

data Nine

instance s9 :: Sized Nine where
  sized _ = 9

type Vec9 a = Vec Nine a

data IdxNine = IdxNine0 | IdxNine1 | IdxNine2 | IdxNine3 | IdxNine4 | IdxNine5 | IdxNine6 | IdxNine7 | IdxNine8

idxNineArray =
  [ IdxNine0, IdxNine1, IdxNine2, IdxNine3, IdxNine4, IdxNine5, IdxNine6, IdxNine7, IdxNine8 ]

vec9 :: forall a. a -> a -> a -> a -> a -> a -> a -> a -> a -> Vec9 a
vec9 a b c d e f g h i = Vec [a, b, c, d, e, f, g, h, i]

idxNineToInt :: IdxNine -> Int
idxNineToInt idx =
  case idx of
    IdxNine0 -> 0
    IdxNine1 -> 1
    IdxNine2 -> 2
    IdxNine3 -> 3
    IdxNine4 -> 4
    IdxNine5 -> 5
    IdxNine6 -> 6
    IdxNine7 -> 7
    IdxNine8 -> 8

getNine :: forall a. IdxNine -> Vec9 a -> a
getNine idx (Vec v) = unsafePartial $ unsafeIndex v (idxNineToInt idx)

setNine :: forall a. IdxNine -> a -> Vec9 a -> Vec9 a
setNine idx n (Vec v) = Vec (unsafePartial $ fromJust (insertAt (idxNineToInt idx) n v))

--------------------------------------------------------------------------------
-- Vec'
--------------------------------------------------------------------------------

newtype Vec' s a = Vec' (Array a)

vec0' :: forall a. Vec' D1 a
vec0' = Vec' [ ]

vec1' :: forall a. a -> Vec' D1 a
vec1' x1 = Vec' [ x1 ]

vec2' :: forall a. a -> a -> Vec' D2 a
vec2' x1 x2 = Vec' [ x1, x2 ]

vec3' :: forall a. a -> a -> a -> Vec' D3 a
vec3' x1 x2 x3 = Vec' [ x1, x2, x3 ]

vec4' :: forall a. a -> a -> a -> a -> Vec' D4 a
vec4' x1 x2 x3 x4 = Vec' [ x1, x2, x3, x4 ]

-- ...

vec9' :: forall a. a -> a -> a -> a -> a -> a -> a -> a -> a -> Vec' D9 a
vec9' x1 x2 x3 x4 x5 x6 x7 x8 x9 = Vec' [ x1, x2, x3, x4, x5, x6, x7, x8, x9 ]

get' :: forall i s a. Nat i => Nat s => Lt i s => i -> Vec' s a -> a
get' i (Vec' xs) = unsafePartial $ unsafeIndex xs (toInt i)

set' :: forall i s a. Nat i => Nat s => Lt i s => i -> a -> Vec' s a -> Vec' s a
set' i val (Vec' xs) = Vec' (unsafePartial $ fromJust (insertAt (toInt i) val xs))


getX' :: forall s a. Nat s => Lt D0 s => Vec' s a -> a
getX' x = get' d0 x

getY' :: forall s a. Nat s => Lt D1 s => Vec' s a -> a
getY' x = get' d1 x

getZ' :: forall s a. Nat s => Lt D2 s => Vec' s a -> a
getZ' x = get' d2 x

getU' :: forall s a. Nat s => Lt D3 s => Vec' s a -> a
getU' x = get' d3 x


derive instance genericVec' :: Generic (Vec' s a) _

instance showVec' :: Show a => Show (Vec' s a) where
  show = genericShow

instance eqVec' :: Eq a => Eq (Vec' s a) where
  eq = genericEq

instance ordVec' :: Ord a => Ord (Vec' s a) where
  compare = genericCompare

instance semigroupVec' :: Semigroup (Vec' s a) where
  append = genericAppend

instance monoidVec' :: Monoid (Vec' s a) where
  mempty = genericMempty

--------------------------------------------------------------------------------
-- TEST
--------------------------------------------------------------------------------
