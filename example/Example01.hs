module Example01 where

import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Data.IORef

import Data.SizedVector
import Data.HList

import Graphics.LambdaGL.Buffer
import Graphics.LambdaGL.Program
import Graphics.LambdaGL.Draw
import Graphics.LambdaGL.Shader
import Graphics.LambdaGL.Texture
import Graphics.LambdaGL.Uniform
import Graphics.LambdaGL.Types.Shared

example01 :: IO (IO ())
example01 = do
  -- Timer to use as a uniform.
  timer <- newIORef 0
  draw <- createDrawAction
  pure $ readIORef timer >>= draw >> modifyIORef timer (+1)

createDrawAction :: IO (Int -> IO ())
createDrawAction = do
  vertexBuffer :: Buffer '[(D2, Float)] <- createBuffer
  writeBuffer vertexBuffer $ VS.fromList $ [HostData (V2 (-1) (-1)) <:> Nil, HostData (V2 (1) (-1)) <:> Nil, HostData (V2 1 1) <:> Nil, HostData (V2 (-1) 1) <:> Nil, HostData (V2 (-1) (-1)) <:> Nil]

  vertexShader :: CompiledShader 'Vertex '[] '[] '[(D2, Float)] '[(D2, Float)] <- compileShader $ Shader $ ShaderSource vertexShader

  fragmentShader :: CompiledShader 'Fragment '[Named "time" Float] '[Texture NoMipMap TextureNormal D2 RGBA8] '[(D2, Float)] '[] <- compileShader $ Shader $ ShaderSource fragmentShader

  texture <- newTexture @NoMipMap @TextureNormal @D2 @RGBA8
  
  bindTexture texture $ do
    writeTexture (V2 2 2) $ VS.fromList [V4 0 0 0 255, V4 255 255 255 255, V4 255 255 255 255, V4 0 0 0 255]

  program <- createProgram $ TwoShader vertexShader fragmentShader

  let uniforms x =
          (makeDrawUniform $ (Named x :: Named "time" Float))

  let initial = toDrawInput vertexBuffer 0
  drawObj <- createDraw program (uniforms 0) initial
  pure $ \i -> do
    let pI = skipDrawInput @'[(D2, Float)]
    runDrawEnv $ do
      clearColor $ V4 1 1 1 1
      bindLastTexture texture $
        draw drawObj (uniforms $ fromIntegral i) pI (DrawOptions TriangleStrip 0 5)
    pure ()

fragmentShader :: B.ByteString
fragmentShader =
  mconcat . fmap (<> "\n") $
    [ "#version 450 core",
      "in  vec2 fragCoord;",
      "out vec4 fragColor;",
      "uniform float time;",
      "uniform sampler2D tex;",
      "void main()",
      "{",
      "vec4 c = texture(tex, fragCoord);",
      "fragColor = vec4(abs(fragCoord) * (sin(time/21) * sin(time/13) + abs(vec2(sin(time/47)))), c.z, 1.0);",
      "}"
    ]

vertexShader :: B.ByteString
vertexShader =
  mconcat . fmap (<> "\n") $
    [ "#version 430 core",
      "layout(location = 0) in vec2 vPosition;",
      "out vec2 fragCoord;",
      "void main()",
      "{",
      "gl_Position = vec4(vPosition, 1, 1);",
      "fragCoord = vPosition;",
      "}"
    ]
