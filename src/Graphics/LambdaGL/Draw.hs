module Graphics.LambdaGL.Draw where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.StateVar as SV
import Data.Proxy
import GHC.TypeLits
import Control.Monad.IO.Class
import Data.Word
import Foreign.Ptr
import Data.Coerce

import Data.SizedVector

import Graphics.LambdaGL.Types.List
import Graphics.LambdaGL.Types.Shared
import Graphics.LambdaGL.Program
import Graphics.LambdaGL.Buffer
import Graphics.LambdaGL.Uniform
import Graphics.LambdaGL.Texture

type Stride = Int
type OffsetWithinStride = Int
type OffsetInStrides = Int
type InputLocation = Word32

data DrawInput complete (i :: [*]) = DrawInput [(InputLocation -> IO ())]
data DrawUniform complete u = DrawUniform (GL.Program -> IO ())
data DrawOptions = DrawOptions ProgramShape Int Int deriving (Show)
data ProgramShape = Triangles | TriangleStrip deriving (Show)

data Draw u tex i where
  Draw :: GL.VertexArrayObject -> (DrawUniform c1 u -> DrawInput c2 i -> DrawOptions -> IO ()) -> Draw u tex i

newtype DrawEnv (tex :: [*]) m a = DrawEnv (m a) deriving (Functor, Applicative, Monad)

data Complete
data NotComplete

type family MergeComplete a b where
  MergeComplete Complete Complete = Complete
  MergeComplete _ _ = NotComplete

toDrawInput :: forall f a. (BuildDrawInput '[] a) => Buffer a -> Int -> DrawInput Complete a
toDrawInput (Buffer glBuffer) offset =
  let (DrawInput inputs) =
        buildDrawInput'
          (DrawInput [] :: DrawInput Complete '[])
          glBuffer
          (Proxy @a)
          offset
          0
   in DrawInput inputs

mergeDrawInputs :: DrawInput c1 a -> DrawInput c2 b -> DrawInput (MergeComplete c1 c2) (Merge a b)
mergeDrawInputs (DrawInput a) (DrawInput b) = DrawInput (a <> b)

skipDrawInput :: forall i. SkipDrawInput i => DrawInput NotComplete i
skipDrawInput = skipDrawInput' (Proxy @i)

class SkipDrawInput i where
  skipDrawInput' :: Proxy i -> DrawInput NotComplete i

instance SkipDrawInput '[] where
  skipDrawInput' _ = DrawInput []

instance SkipDrawInput as => SkipDrawInput (a : as) where
  skipDrawInput' _ =
    let (DrawInput list) = skipDrawInput' (Proxy @as)
     in DrawInput ((\_ -> pure ()) : list)

class BuildDrawInput (i :: [*]) (xs :: [*]) where
  buildDrawInput' :: DrawInput Complete i -> GL.BufferObject -> Proxy xs -> Int -> Int -> DrawInput Complete (AppendMerge i xs)

instance BuildDrawInput i '[] where
  buildDrawInput' pI _ _ _ _ = pI

instance (BuildDrawInput (Append i (v, x)) xs, GetDataType x, GetDimension v, GetSize i, GetSize ((v, x) : xs)) => BuildDrawInput i ((v, x) : xs) where
  buildDrawInput' (DrawInput list) glBuffer remainingElements offset stride =
    let dataType = getDataType (Proxy :: Proxy x) :: GL.DataType
        strideOffset = getSize (Proxy :: Proxy i)
        dimension = (getDimension (Proxy :: Proxy v))
        newEntry = setInput glBuffer (toEnum dimension) dataType stride strideOffset offset
     in buildDrawInput'
          ((DrawInput (list ++ [newEntry])) :: DrawInput f (Append i ((v, x))))
          glBuffer
          (Proxy @xs)
          offset
          stride
    where
      setInput :: GL.BufferObject -> Int -> GL.DataType -> Stride -> OffsetWithinStride -> OffsetInStrides -> Word32 -> IO ()
      setInput glBuffer dimension dataType stride strideOffset offset position = do
        GL.bindBuffer GL.ArrayBuffer SV.$= Just glBuffer
        GL.vertexAttribPointer (coerce position)
          SV.$= (GL.ToFloat, GL.VertexArrayDescriptor (toEnum dimension) dataType (toEnum stride) (plusPtr nullPtr $ offset * stride + strideOffset))
        GL.vertexAttribArray (coerce position) SV.$= GL.Enabled

emptyDrawUniform :: DrawUniform Complete []
emptyDrawUniform = DrawUniform (\_ -> pure ())

mergeDrawUniforms :: DrawUniform c1 u1 -> DrawUniform c2 u2 -> DrawUniform (MergeComplete c1 c2) (Unique (Merge u1 u2))
mergeDrawUniforms (DrawUniform u1) (DrawUniform u2) = DrawUniform (u1 <> u2)

makeDrawUniform :: IsUniform uniform => uniform -> DrawUniform Complete '[uniform]
makeDrawUniform uniform = DrawUniform $ \glProgram -> do
  location <- SV.get (GL.uniformLocation glProgram (getName uniform))
  GL.uniform location SV.$= toGLUniform uniform

draw :: (Contains u1 u2 ~ True, Contains i1 i2 ~ True, MonadIO m) => Draw u2 tex i2 -> DrawUniform c1 u1 -> DrawInput c2 i1 -> DrawOptions -> DrawEnv tex m ()
draw (Draw _ f) (DrawUniform uniforms) (DrawInput inputs) =
  DrawEnv . liftIO . f (DrawUniform uniforms) (DrawInput inputs)


createDraw :: Program u tex i -> DrawUniform Complete u -> DrawInput Complete i -> IO (Draw u tex i)
createDraw (Program glProgram) (DrawUniform setUniforms) (DrawInput input) = do
  GL.currentProgram SV.$= Just glProgram
  vertexArray <- GL.genObjectName
  GL.bindVertexArrayObject SV.$= Just vertexArray
  setUniforms glProgram
  setInput input 0
  pure $ Draw vertexArray $ \(DrawUniform updateUniforms) (DrawInput updatedInputs) drawOptions -> do
    GL.bindVertexArrayObject SV.$= Just vertexArray
    GL.currentProgram SV.$= Just glProgram
    updateUniforms glProgram
    setInput updatedInputs 0
    drawShapes drawOptions
  where
    setInput :: [InputLocation -> IO ()] -> InputLocation -> IO ()
    setInput [] _ = pure ()
    setInput (x : xs) inputLocation = x inputLocation *> setInput xs (succ inputLocation)
    drawShapes :: DrawOptions -> IO ()
    drawShapes (DrawOptions shape startIndex endIndex) = do
      let glShape =
            case shape of
              Triangles -> GL.Triangles
              TriangleStrip -> GL.TriangleStrip
      GL.drawArrays glShape (toEnum startIndex) (toEnum endIndex)

freeDraw :: Draw u tex i -> IO ()
freeDraw (Draw glVertexArrayObject _) = GL.deleteObjectName glVertexArrayObject


runDrawEnv :: MonadIO m => DrawEnv '[] m a -> m a
runDrawEnv (DrawEnv action) = action

bindLastTexture :: forall t dim c tex mipMap m a. (MonadIO m, CountElements tex, Last tex ~ Texture mipMap t dim c) => Texture mipMap t dim c -> DrawEnv tex m a -> DrawEnv (Init tex) m a
bindLastTexture (Texture glTexture) (DrawEnv m) =
  let textureLocation = countElements (Proxy @tex) - 1
   in DrawEnv $ do
        liftIO $ do
          GL.activeTexture SV.$= GL.TextureUnit (toEnum textureLocation)
          GL.textureBinding GL.Texture2D SV.$= Just glTexture
        m

bindTextureAt :: forall i t1 dim1 c1 tex mipMap1 m a mipMap2 t2 dim2 c2. (TextureClass mipMap1 t1 dim1 c1, TextureClass mipMap2 t2 dim2 c2, MonadIO m, KnownNat i, Texture mipMap2 t2 dim2 c2 ~ AtIndex tex i) => 
  Texture mipMap1 t1 dim1 c1 -> Proxy i -> DrawEnv (ReplaceAt tex i (Texture mipMap1 t1 dim1 c1)) m a -> DrawEnv tex m a
bindTextureAt (Texture glTexture) _ (DrawEnv m) = 
  let textureLocation = fromIntegral $ natVal (Proxy @i)
  in DrawEnv $ do
      beforeBound <- liftIO $ do
        GL.activeTexture SV.$= GL.TextureUnit (toEnum textureLocation)
        SV.get $ GL.textureBinding (getGLTarget (Proxy @(Texture mipMap2 t2 dim2 c2)))
      liftIO $ GL.textureBinding (getGLTarget (Proxy @(Texture mipMap1 t1 dim1 c1))) SV.$= Just glTexture
      a <- m
      liftIO $ do
        GL.activeTexture SV.$= GL.TextureUnit (toEnum textureLocation)
        GL.textureBinding (getGLTarget (Proxy @(Texture mipMap2 t2 dim2 c2))) SV.$= beforeBound
      pure a

clearColor :: MonadIO m => V4 Float -> DrawEnv x m ()
clearColor (V4 r g b a) = DrawEnv . liftIO $ do
  GL.clearColor SV.$= GL.Color4 r g b a
  GL.clear [GL.ColorBuffer]