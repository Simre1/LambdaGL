module Graphics.LambdaGL.Shader where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Proxy
import qualified Data.StateVar as SV
import Control.Exception (bracketOnError)

import Graphics.LambdaGL.Utility


data ShaderType = Vertex | Fragment

class GetShaderType (t :: ShaderType) where
  getShaderType :: Proxy t -> ShaderType

instance GetShaderType 'Vertex where
  getShaderType _ = Vertex

instance GetShaderType 'Fragment where
  getShaderType _ = Fragment
data ShaderSource = ShaderSource B.ByteString

data Shader (t :: ShaderType) (u :: [*]) (tex :: [*]) (i :: [*]) (o :: [*]) = Shader ShaderSource

data CompiledShader (t :: ShaderType) (u :: [*]) (tex :: [*]) (i :: [*]) (o :: [*]) = CompiledShader GL.Shader

compileShader :: forall t u i o tex. GetShaderType t => Shader t u tex i o -> IO (CompiledShader t u tex i o)
compileShader (Shader (ShaderSource bytestring)) = do
  GL.createShader (toGLShaderType $ getShaderType (Proxy :: Proxy (t :: ShaderType))) `bracketOnError` GL.deleteObjectName $ \shader -> do
    GL.shaderSourceBS shader SV.$= bytestring
    compileAndCheck shader
    pure $ CompiledShader shader
  where
    toGLShaderType :: ShaderType -> GL.ShaderType
    toGLShaderType (Vertex) = GL.VertexShader
    toGLShaderType (Fragment) = GL.FragmentShader

freeCompiledShader :: CompiledShader t u tex i o -> IO ()
freeCompiledShader (CompiledShader glShader) = GL.deleteObjectName glShader

compileAndCheck :: GL.Shader -> IO ()
compileAndCheck = checked GL.compileShader GL.compileStatus GL.shaderInfoLog "compile"
