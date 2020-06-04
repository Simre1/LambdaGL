module Graphics.LambdaGL.Program where

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.LambdaGL.Shader
import Graphics.LambdaGL.Utility (checked)
import Graphics.LambdaGL.Types.List
import Control.Exception (bracketOnError)

data ProgramAssembler (u :: [*]) (tex :: [*]) (i :: [*]) where
  TwoShader :: (CompiledShader 'Vertex u1 tex1 i l) -> (CompiledShader 'Fragment u2 tex2 l x) -> ProgramAssembler (Unique (Merge u1 u2)) (Expand tex1 tex2) i

assembleProgram ::
  CompiledShader Vertex u1 tex1 i o -> CompiledShader Fragment u2 tex2 o '[] -> ProgramAssembler (Unique (Merge u1 u2)) (Expand tex1 tex2) i
assembleProgram = TwoShader

data Program (u :: [*]) (tex :: [*]) (i :: [*]) = Program GL.Program

createProgram :: ProgramAssembler u tex i -> IO (Program u tex i)
createProgram (TwoShader (CompiledShader vertex) (CompiledShader fragment)) = do
  GL.createProgram `bracketOnError` GL.deleteObjectName $ \program -> do
    GL.attachShader program vertex
    GL.attachShader program fragment
    linkAndCheck program
    return $ Program program

freeProgram :: Program u tex i -> IO ()
freeProgram (Program glProgram) = GL.deleteObjectName glProgram

linkAndCheck :: GL.Program -> IO ()
linkAndCheck = checked GL.linkProgram GL.linkStatus GL.programInfoLog "link"
