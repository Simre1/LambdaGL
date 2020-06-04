module Data.StateAction where

import qualified Data.StateVar as SV

data StateAction m a = StateAction (a -> m ()) (m a)

get :: StateAction m a -> m a
get (StateAction _ g) = g

set :: StateAction m a -> a -> m ()
set (StateAction s _) = s

(%=) :: StateAction m a -> a -> m ()
(%=) = set

makeStateAction :: (a -> m ()) -> (m a) -> StateAction m a
makeStateAction = StateAction

mapStateActionM :: Monad m => StateAction m a -> (a -> m b) -> (b -> m a) -> StateAction m b
mapStateActionM (StateAction s g) ab ba = StateAction (\b -> ba b >>= s) (g >>= ab)

mapStateAction :: Functor m => StateAction m a -> (a -> b) -> (b -> a) -> StateAction m b
mapStateAction (StateAction s g) ab ba = StateAction (s . ba) (ab <$> g)

mapActionType :: StateAction m1 a -> (forall x. m1 x -> m2 x) -> StateAction m2 a
mapActionType (StateAction s g) f = StateAction (f . s) (f g)

wrapStateVar :: SV.StateVar a -> StateAction IO a
wrapStateVar statevar = makeStateAction (statevar SV.$=) (SV.get statevar)