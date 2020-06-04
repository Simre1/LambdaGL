module Graphics.LambdaGL.Types.Shared where

import Data.Proxy
import Data.SizedVector
import Data.Word
import Foreign.Storable (sizeOf)

import qualified Graphics.Rendering.OpenGL as GL

data D1

data D2

data D3

data D4

type family HostFormat a where
  HostFormat Float = Float
  HostFormat Int = Int
  HostFormat Word8 = Word8
  HostFormat ((D1, d)) = V1 (HostFormat d)
  HostFormat ((D2, d)) = V2 (HostFormat d)
  HostFormat ((D3, d)) = V3 (HostFormat d)
  HostFormat ((D4, d)) = V4 (HostFormat d)

class GetDimension a where
  getDimension :: Proxy a -> Int

instance GetDimension D1 where
  getDimension _ = 1

instance GetDimension D2 where
  getDimension _ = 2

instance GetDimension D3 where
  getDimension _ = 3

instance GetDimension D4 where
  getDimension _ = 4

class GetDataType x where
  getDataType :: Proxy x -> GL.DataType

instance GetDataType Float where
  getDataType _ = GL.Float

class GetSize (x :: [*]) where
  getSize :: Proxy x -> Int

instance GetSize '[] where
  getSize _ = 0

instance (GetSize r, GetDimension d) => GetSize ((d, Float) : r) where
  getSize _ = getDimension (Proxy @d) * sizeOf (undefined :: Float) + getSize (Proxy @r)