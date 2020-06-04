module Graphics.LambdaGL.Buffer where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector.Storable as VS
import Foreign.Storable
import Data.StateVar

import Data.HList
import Graphics.LambdaGL.Types.Shared

newtype HostData a = HostData (HostFormat a)

deriving instance Storable (HostFormat a) => Storable (HostData a)

data Buffer (a :: [*]) = Buffer GL.BufferObject

createBuffer :: IO (Buffer a)
createBuffer =
  Buffer <$> GL.genObjectName

freeBuffer :: Buffer a -> IO ()
freeBuffer (Buffer glBuffer) = GL.deleteObjectName glBuffer

writeBuffer :: forall a. Storable (HList HostData a) => Buffer a -> VS.Vector (HList HostData a) -> IO ()
writeBuffer (Buffer glBuffer) bufferData = do
  GL.bindBuffer GL.ArrayBuffer $= Just glBuffer
  VS.unsafeWith bufferData $ \ptr -> do
    GL.bufferData GL.ArrayBuffer $= (toEnum bufferSize, ptr, GL.StaticDraw)
  where
    bufferSize :: Int
    bufferSize = VS.length bufferData * sizeOf (undefined :: HList HostData a)
