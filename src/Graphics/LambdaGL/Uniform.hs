module Graphics.LambdaGL.Uniform where

import qualified Graphics.Rendering.OpenGL as GL
import GHC.TypeLits
import Data.Proxy

data Named (l :: Symbol) a = Named a

data Uniform a = IsUniform a => Uniform a

class GL.Uniform (GLUniform a) => IsUniform a where
  type GLUniform a
  toGLUniform :: a -> GLUniform a
  getName :: a -> String

instance (KnownSymbol x) => IsUniform (Named x Float) where
  type GLUniform (Named x Float) = Float
  toGLUniform (Named a) = a
  getName _ = symbolVal (Proxy @x)