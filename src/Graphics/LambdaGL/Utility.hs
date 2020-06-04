module Graphics.LambdaGL.Utility where

import qualified Data.StateVar as SV
import Control.Monad (unless)

checked ::
  (t -> IO ()) ->
  (t -> SV.GettableStateVar Bool) ->
  (t -> SV.GettableStateVar String) ->
  String ->
  t ->
  IO ()
checked action getStatus getInfoLog message object = do
  action object
  ok <- SV.get (getStatus object)
  unless ok $ do
    infoLog <- SV.get (getInfoLog object)
    fail (message ++ " log: " ++ infoLog)

