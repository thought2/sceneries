module Main where

import Color (Color, graytone, toRGBA')
import Control.Apply (lift2)
import Control.Monad.Aff (Aff, Canceler, launchAff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Array (concatMap, drop, filter, foldr, length, mapWithIndex, range, snoc, take, zipWith)
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Array.NonEmpty as NE
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either, either, note)
import Data.Foreign (Foreign)
import Data.Foreign.Class (decode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.List.NonEmpty as NEList
import Data.Matrix (toArray) as M
import Data.Matrix4 (identity, makePerspective, translate) as M
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, maybe')
import Data.Midi (Event(..), TimedEvent(..))
import Data.Midi.WebMidi (createEventChannel)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (between)
import Data.Record.Builder (build)
import Data.Record.Builder as RB
import Data.Ring (class Ring)
import Data.Semigroup.Foldable (foldMap1)
import Data.String (Pattern(..), split)
import Data.String as Str
import Data.String.NonEmpty (unsafeFromString)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (class Nat, D3, D9, d0, d1, d2, d3, d4, d5, d6, d7, d8)
import Data.Vec (Vec, replicate', toArray, (!!))
import Data.Vec (index, snoc, toArray, updateAt) as Vec
import Data.Vec.Extra (Vec1, Vec2, Vec3, vec2, vec3)
import Data.Vec.Extra (uncurry2) as Vec
import Data.Vector3 as Vector3
import Debug.Trace (spy)
import Extensions (fail, mapM)
import Graphics.WebGLAll (Attribute, Capacity(DEPTH_TEST), EffWebGL, Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT), Mat4, Mode(TRIANGLES), Shaders(Shaders), Uniform, WebGLContext, WebGLProg, WebGl, clear, clearColor, drawArr, enable, getCanvasHeight, getCanvasWidth, makeBufferFloat, runWebGL, setUniformFloats, viewport, withShaders)
import Graphics.WebGLAll as Gl
import Math (cos, pi, sin, (%))
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Ajax
import Partial.Unsafe (unsafePartial)
import Pathy (class IsDirOrFile, class IsRelOrAbs, Dir, Path, Rel, RelDir, RelFile, SandboxedPath, dir, extension, file, fileName, parseRelFile, posixParser, posixPrinter, sandboxAny, unsafePrintPath, (</>))
import Prelude (class Functor, class Semigroup, class Show, Unit, bind, const, discard, flip, id, map, max, negate, not, one, otherwise, pure, show, sub, unit, zero, (#), ($), (*), (+), (-), (/), (<), (<#>), (<$>), (<*>), (<<<), (<>), (==), (>>=), (>>>))
import Signal (Signal, filterMap, foldp, map2, merge, mergeMany, runSignal, (~>))
import Signal.Channel (CHANNEL, subscribe)
import Signal.DOM (DimensionPair, CoordinatePair, animationFrame, keyPressed, mousePos, windowDimensions)
import Signal.Time (Time)
import WFObjFormat as WFO

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
  sigInputKeyboardMouse <- keyboardMouseInput <#> map SigInput
  sigInputMidi <- midiInput <#> map SigInput

  let signals = sigTime <> sigSize <> sigInputKeyboardMouse <> sigInputMidi
      sigState = foldp update (initState config) signals

  debug config signals sigState
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
    f (TimedEvent { event : (Just (ControlChange _ n pct)) })
      | between 14 22 n = Just $ Input (n - 14) (reMap' pct)
    f _ = Nothing
    reMap' n = reMap (vec2 zero 127.0) spaceUnit (toNumber n)

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
    let n = map (length <<< unwrap) scenes # foldr max 0
    size <- getCanvasSize canvasContext
    randomField <- range 0 n # mapM (const randVec3)
    pure $ DynConfig
      { randomField
      , scenes
      , size
      , bindings
      }
  where
    { fieldZ } = staticConfig'
    randVec2 = randomVec <#> map (reMap spaceUnit spaceUnitSigned)
    randVec3 = lift2 Vec.snoc (pure fieldZ) randVec2

getCanvasSize :: forall eff. WebGLContext -> Eff ( webgl :: WebGl | eff ) Vec2i
getCanvasSize ctx =
  lift2 vec2 (getCanvasWidth ctx) (getCanvasHeight ctx)

randomVec :: forall eff s. Nat s => EffRandom eff (Vec s Number)
randomVec = pure random # traverse id

--------------------------------------------------------------------------------
-- MORPH CHAIN
--------------------------------------------------------------------------------

instance semigroupScaleFn :: Semigroup (ScaleFn a) where
  append (ScaleFn t1 f1) (ScaleFn t2 f2) = ScaleFn (t1 + t2) f
    where
      f t | t < t1    = f1 t
          | otherwise = f2 (t - t1)

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
    { pcts : replicate' 0.5
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

    SigInput (Input 0 pct) -> State $ st { pcts = Vec.updateAt d0 pct st.pcts }
    SigInput (Input 1 pct) -> State $ st { pcts = Vec.updateAt d1 pct st.pcts }
    SigInput (Input 2 pct) -> State $ st { pcts = Vec.updateAt d2 pct st.pcts }
    SigInput (Input 3 pct) -> State $ st { pcts = Vec.updateAt d3 pct st.pcts }
    SigInput (Input 4 pct) -> State $ st { pcts = Vec.updateAt d4 pct st.pcts }
    SigInput (Input 5 pct) -> State $ st { pcts = Vec.updateAt d5 pct st.pcts }
    SigInput (Input 6 pct) -> State $ st { pcts = Vec.updateAt d6 pct st.pcts }
    SigInput (Input 7 pct) -> State $ st { pcts = Vec.updateAt d7 pct st.pcts }
    SigInput (Input 8 pct) -> State $ st { pcts = Vec.updateAt d8 pct st.pcts }

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
    cos' = reMap spaceUnit (vec2 pi (two * pi)) >>> cos >>> reMap spaceUnitSigned spaceUnit
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
renderScene config @ (Config { bindings, scenes, randomField }) state @ (State { pcts, time }) = do
    setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix)

    --buf <- makeBufferFloat (spy xs)
--    buf <- makeBufferFloat [0.0, 0.0, 0.0,  1.0, 1.0, 0.0,  0.0, 1.0, 0.0]
    -- buf <- makeBufferFloat [0.8, 0.4629670299649613, 0.0,
    --                         -0.9919921163140466, -1.2603307013877343, 0.0,
    --                         2.3346792446737576, 2.06634065960007, 0.0]
    buf <- makeBufferFloat xsScenes'
    drawArr TRIANGLES buf bindings.aVertexPosition

    where

      xsScenes' :: Array Number
      xsScenes' = do
        (Scene xs) <- NE.toArray scenes
        (Triangle p1 p2 p3) <- xs
        Vec.toArray p1 <> Vec.toArray p2 <> Vec.toArray p3

      xsField = concatMap Vec.toArray randomField

      ScaleFn dur scaleFn = selectScaleFn config state

      velocity = get d0 pcts

      t = sin ((time / maxLoopTime) * two * pi)
          # reMap spaceUnitSigned spaceUnit

      t' = get d1 pcts

      t'' = (time / (velocity * maxLoopTime)) % 1.0

      xs = do
        let (Scene tris) = scaleFn (t'' * dur)
        (Triangle p1 p2 p3) <- tris
        concatMap toArray [ p1, p2, p3 ]

      maxLoopTime = 10.0 * 1000.0

      getPct offsetPct =
        sin ((time / maxLoopTime * two * pi) + (offsetPct * maxLoopTime))
          # reMap spaceUnit spaceUnitSigned

      mvMatrix =
        M.translate (Vector3.vec3 0.0 0.0 (-20.0)) M.identity


renderPerspective :: RenderFn
renderPerspective (Config { bindings }) (State { size, pcts }) = do
  viewport 0 0 (size !! d0) (size !! d1)
  setUniformFloats bindings.uPMatrix (M.toArray pMatrix)
  where
    { zNear, zFar } = staticConfig'
    aspect = Vec.uncurry2 (/) (map toNumber size)
    pMatrix = M.makePerspective
              45.0 --(reMap (vec2 0.0 1.0) (vec2 0.0 360.0) (get d4 pcts))
              aspect --(reMap (vec2 0.0 1.0) (vec2 0.0 5.0) (get d5 pcts))
              zNear
              zFar

renderBackground :: RenderFn
renderBackground _ (State { pcts }) =
  let { r, g, b, a } = toRGBA' (graytone (pcts !! d6)) in
  clearColor r g b a

type RenderFn = forall eff a. Config -> State -> EffWebGL eff Unit
type RenderFn1 = forall eff a. Config -> State -> a -> EffWebGL eff Unit

--------------------------------------------------------------------------------
-- DATA GET
--------------------------------------------------------------------------------

data ErrGet = ErrGetIndex

errorsGet = { noScenes: "no scenes" }

getScenes :: forall e . Aff (ajax:: AJAX | e) (NonEmptyArray Scene)
getScenes = do
  paths <- getIndex dataDir <#> filter predFn
  mapM getScene paths >>= fromArray >>> maybe' (\_ -> fail errorsGet.noScenes) pure
  where
      predFn = fileName >>> extension >>> map (_ == objExt) >>> fromMaybe false
      objExt = unsafePartial $ unsafeFromString "obj"
      { dataDir } = staticConfig'

getIndex :: forall e . RelDir -> Aff (ajax:: AJAX | e) (Array RelFile)
getIndex folder =
  Ajax.get (printPath' (sandboxAny path))
  >>= (_.response >>> parseIndexFile >>> either fail pure)
  <#> map (folder </> _)
  where
      path = folder </> indexFile
      indexFile = file (SProxy :: SProxy "index.txt")

getScene :: forall e . RelFile -> Aff (ajax:: AJAX | e) Scene
getScene path =
  Ajax.get (printPath' $ sandboxAny path)
  >>= ( _.response
        >>> readWFObj
        >>> decode
        >>> runExcept
        >>> either
              (fail <<< show <<< NEList.head)
              pure
      )
  >>= ( parseWFOMain_
        >>> either (fail <<< (\x -> "here" <> x) <<< show) (\(File x) -> pure $ Scene $ f x)
      )

  where
    f models = do
      (Model m) <- models
      (Face tris) <- m
      tris


--------------------------------------------------------------------------------
-- DATA PARSE
--------------------------------------------------------------------------------

newtype File = File (Array Model)

newtype Model = Model (Array Face)

newtype Face = Face (Array Triangle)

spy' s x = spy (s /\ x) # Tuple.snd

parseWFOMain_ :: WFO.Main -> Either ErrParse File
parseWFOMain_ (WFO.Main { models }) =
  File <$> traverse (parseWFOModel_ verticesLookup) models
  where
    verticesLookup = models
      >>= (unwrap >>> _.vertices)
      >>= (parseWFOVertex_ >>> pure)

parseWFOVertex_ :: WFO.Vertex -> Vec3n
parseWFOVertex_ (WFO.Vertex { x, y, z }) = vec3 x y z

parseWFOModel_ :: Array Vec3n -> WFO.Model -> Either ErrParse Model
parseWFOModel_ verticesLookup (WFO.Model { vertices, faces }) =
  Model <$> traverseWithIndex (\i -> parseWFOFace_ i verticesLookup) faces

parseWFOFace_ :: Int -> Array Vec3n -> WFO.Face -> Either ErrParse Face
parseWFOFace_ i verticesLookup (WFO.Face { vertices }) =
  note (ErrParseTriangle verticesLookup vertices i) $ do
  vertices' <- (traverse (\i -> verticesLookup Arr.!! (i - 1)) indices ) :: Maybe (Array Vec3n)

  a <- vertices' Arr.!! 0
  b <- vertices' Arr.!! 1
  c <- vertices' Arr.!! 2
  d <- vertices' Arr.!! 3

  Just $ Face [ Triangle a b c, Triangle a c d ]

  where
    indices = map (unwrap >>> _.vertexIndex) vertices

parseIndexFile :: String -> Either String (Array RelFile)
parseIndexFile str =
  split (Pattern "\n") str
  # filter (not <<< Str.null)
  # traverse (parseRelFile posixParser)
  # note errMsg
  where
    errMsg = "Invalid index file"

data ErrParse
  = ErrParseLookup
  | ErrParseTriangle (Array Vec3n) (Array WFO.VertexRef) Int

-- data DebugSeg = DSFile String | DSModel Int | DSFace Int | DSVertex Int
-- data DebugPath = Array DebugSeg

-- data E = E DebugPath Err

derive instance genericErrParse :: Generic ErrParse _

instance showErrParse :: Show ErrParse where
  show = genericShow

--------------------------------------------------------------------------------
-- SHORTHANDS
--------------------------------------------------------------------------------

two :: Number
two = 2.0

spaceCircle :: Vec2n
spaceCircle = vec2 zero (two * pi)

spaceUnit :: forall a. Ring a => Vec2 a
spaceUnit = vec2 zero one

spaceUnitSigned :: forall a. Ring a => Vec2 a
spaceUnitSigned = vec2 (-one) one

--------------------------------------------------------------------------------
-- UTIL
--------------------------------------------------------------------------------

printPath' :: forall a b.
  IsRelOrAbs a => IsDirOrFile b => SandboxedPath a b -> String
printPath' path = unsafePrintPath posixPrinter path

reMap :: Vec2n-> Vec2n -> Number -> Number
reMap pair1 pair2 value =
  pivot2 + (pct * dist2)
  where
    pivot1 = pair1 !! d0
    pivot2 = pair2 !! d0
    dist1 = Vec.uncurry2 sub pair1
    dist2 = Vec.uncurry2 sub pair2
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

f :: StaticConfig -> DynConfig -> Config
f (StaticConfig x) (DynConfig y) =
  Config $ mergeRecords x y

mergeRecords :: forall r1 r2 r3. Union r2 r3 r1 => { | r3 } -> { | r2 } -> { | r1 }
mergeRecords r1 r2 = build (RB.merge r1) r2

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

instance morphVec3 :: Morph a => Morph (Vec D3 a) where
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

derive instance newtypeConfig :: Newtype Config _

newtype State = State
  { pcts :: Vec D9 Number
  , time :: Number
  , size :: Vec2i
  }

newtype Scene = Scene (Array Triangle)

derive instance newtypeScene :: Newtype Scene _

data Triangle = Triangle Vec3n Vec3n Vec3n
newtype Triangle' = Triangle' (Vec3 Vec3n)


type Vec1i = Vec1 Int
type Vec2i = Vec2 Int
type Vec3i = Vec3 Int

type Vec1n = Vec1 Number
type Vec2n = Vec2 Number
type Vec3n = Vec3 Number

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
-- VECTOR
--------------------------------------------------------------------------------

getX = flip Vec.index d0
getY = flip Vec.index d1
getZ = flip Vec.index d2

get0 = flip Vec.index d0
get1 = flip Vec.index d1
get2 = flip Vec.index d2

set0 = Vec.updateAt d0
set1 = Vec.updateAt d1
set2 = Vec.updateAt d2

get = flip Vec.index

--------------------------------------------------------------------------------
-- TEST
--------------------------------------------------------------------------------

-- instance semiringVec :: (Nat s, Semiring a) => Semiring (Vec s a) where
--   add = lift2 add
--   mul = lift2 mul
--   zero = replicate (toInt (Proxy :: Proxy s) - 1) zero
--   one = replicate (toInt (Proxy :: Proxy s) - 1) one

-- instance natProxy :: Nat (Proxy a) where
--   toInt x = 3
