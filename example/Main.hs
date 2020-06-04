module Main where

import qualified SDL
import qualified Data.StateVar as SV
import qualified Data.Text as T
import qualified Graphics.Rendering.OpenGL as GL
import GHC.Clock
import Control.Concurrent (threadDelay)
import Control.Monad

import Example01

main :: IO ()
main = do
  -- Set up SDL window and GL context.
  SDL.initializeAll
  window <- SDL.createWindow (T.pack "My SDL Application") SDL.defaultWindow {SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL {SDL.glProfile = SDL.Core SDL.Debug 4 6}}
  context <- SDL.glCreateContext window
  GL.debugOutput SV.$= GL.Enabled
  SDL.HintRenderScaleQuality SV.$= SDL.ScaleLinear                  
  renderQuality <- SV.get SDL.HintRenderScaleQuality          
  when (renderQuality /= SDL.ScaleLinear) $                    
    putStrLn "Warning: Linear texture filtering not enabled!"
  
  -- Set up example01
  drawExample <- example01

  -- Start app loop
  time <- getMonotonicTime
  appLoop time drawExample window

appLoop :: Double -> IO () -> SDL.Window -> IO ()
appLoop time drawExample window = do

  -- Poll events to determine if the app should exit.
  events <- SDL.pollEvents
  let checkForExitEvent event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeEscape
          SDL.WindowClosedEvent _ -> True
          _ -> False
      shouldExit = any checkForExitEvent events

  -- Draw the given example.
  drawExample

  -- Calculate used time to loop at 60FPS
  SDL.glSwapWindow window
  time2 <- getMonotonicTime
  threadDelay ((+) 16600 $ round $ (time - time2) * 1000000)
  time3 <- getMonotonicTime

  -- Continue unless quit button was pressed.
  unless shouldExit (appLoop time3 drawExample window)
