module Graphics.LambdaGL.Texture where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.StateVar as SV
import qualified Data.Vector.Storable as VS
import Data.Word
import Foreign.Storable
import Data.Proxy
import Control.Monad.IO.Class
import Foreign.Ptr
import Control.Monad

import Data.SizedVector
import Data.StateAction

import Graphics.LambdaGL.Types.Shared


data Texture m t d c = Texture GL.TextureObject

data TextureNormal = TextureNormal

data TextureArray = TextureArray

data MipMap

data NoMipMap

class HasMipMap m where
  hasMipMap :: Proxy m -> Bool

instance HasMipMap MipMap where
  hasMipMap _ = True

instance HasMipMap NoMipMap where
  hasMipMap _ = False

data TextureFilter
  = Nearest
  | Linear

data Clamping
  = Clamp
  | Repeat
  | ClampToEdge
  | ClampToBorder

data Coordinate
  = CoordinateX
  | CoordinateY
  | CoordinateZ
  | CoordinateW

data Repetition = Repeated | Mirrored

data RGBA8 = RGBA8

data Red = Red

class (GL.BindableTextureTarget (TextureTarget t dim), HasMipMap m, GL.ParameterizedTextureTarget (TextureTarget t dim), GetDimension dim) => TextureClass m t dim color where
  type TextureSize t dim
  type TextureTarget t dim
  type TextureData color
  uploadTexture :: Proxy (Texture m t dim color) -> HostFormat (TextureSize t dim) -> Ptr x -> IO ()
  getGLTarget :: Proxy (Texture m t dim color) -> TextureTarget t dim

instance HasMipMap m => TextureClass m TextureNormal D2 RGBA8 where
  type TextureSize TextureNormal D2 = (D2, Int)
  type TextureData RGBA8 = (D4, Word8)
  type TextureTarget TextureNormal D2 = GL.TextureTarget2D
  uploadTexture _ (V2 x y) dataPtr = do
    let glSize = GL.TextureSize2D (toEnum x) (toEnum y)
        pixelData = GL.PixelData GL.RGBA GL.UnsignedByte dataPtr
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 glSize 0 pixelData
  getGLTarget _ = GL.Texture2D

newtype Bound x m a = Bound (m a) deriving (Functor, Applicative, Monad)

newTexture :: forall m t dim color. (HasMipMap m, TextureClass m t dim color) => IO (Texture m t dim color)
newTexture = do
  texture <- Texture <$> GL.genObjectName
  -- OpenGL defaults to enabling mipmap linear filter. This needs to be corrected for no-mipmap textures
  when (not $ hasMipMap (Proxy @m)) $ bindTexture texture $ textureFilter %= (GL.Linear', GL.Linear')
  pure $ texture

bindTexture :: forall m mipMap t dim color a. (MonadIO m, TextureClass mipMap t dim color) => Texture mipMap t dim color -> Bound (Texture mipMap t dim color) m a -> m a
bindTexture texture@(Texture glTarget) (Bound action) = do
  liftIO $ GL.textureBinding (getGLTarget (Proxy @(Texture mipMap t dim color))) SV.$= Just glTarget
  action

writeTexture :: forall m mipMap t dim color. (MonadIO m, Storable (HostFormat (TextureData color)), TextureClass mipMap t dim color) => HostFormat (TextureSize t dim) -> VS.Vector (HostFormat (TextureData color)) -> Bound (Texture mipMap t dim color) m ()
writeTexture textureSize data' =
  Bound . liftIO $ VS.unsafeWith data' $ \dataPtr -> do
    uploadTexture (Proxy @(Texture mipMap t dim color)) textureSize dataPtr

textureFilter :: forall m mipMap t dim color. (HasMipMap mipMap, MonadIO m, TextureClass mipMap t dim color) => StateAction (Bound (Texture mipMap t dim color) m) (GL.TextureFilter, GL.TextureFilter)
textureFilter = mapActionType (makeStateAction setter getter) $ Bound . liftIO
  where 
    setter (minFilter, magFilter) = do
      if (hasMipMap (Proxy @mipMap))
        then GL.textureFilter (getGLTarget (Proxy @(Texture mipMap t dim color))) SV.$= ((minFilter, Just minFilter), magFilter)
        else GL.textureFilter (getGLTarget (Proxy @(Texture mipMap t dim color))) SV.$= ((minFilter, Nothing), magFilter)
    getter = do
      ((t1,_),t2) <- SV.get (GL.textureFilter (getGLTarget (Proxy @(Texture mipMap t dim color))))
      pure (t1,t2)


textureClamping :: forall m mipMap t dim color. (HasMipMap mipMap, MonadIO m, TextureClass mipMap t dim color) => GL.TextureCoordName -> StateAction (Bound (Texture mipMap t dim color) m) (GL.Repetition, GL.Clamping)
textureClamping coordinate = mapActionType (wrapStateVar stateVar) $ Bound . liftIO 
  where 
    stateVar = GL.textureWrapMode (getGLTarget $ Proxy @(Texture mipMap t dim color)) coordinate
