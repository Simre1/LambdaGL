module Data.HList where

import Data.Proxy
import GHC.TypeLits

import Data.Functor.Identity

import Foreign.Storable
import Foreign.Ptr

data HList f (things :: [*]) where
  Nil  :: HList f '[]
  Cons :: f x -> HList f xs -> HList f (x ': xs)

instance Storable (HList f '[]) where
  sizeOf _ = 0
  alignment _ = 0
  peek _ = pure Nil
  poke _ _ = pure ()

instance (Storable (f x), Storable (HList f xs)) => Storable (HList f (x ': xs)) where
  sizeOf _ = sizeOf (undefined :: f x) + sizeOf (undefined :: HList f xs)
  alignment _ = alignment (undefined :: f x) + alignment (undefined :: HList f xs)
  peek ptr = do
    x <- peek (castPtr ptr) 
    rest <- peek (castPtr (ptr `plusPtr` sizeOf (undefined :: f x)))
    pure $ Cons x rest
  poke ptr (Cons fx rest) = do
    poke (castPtr ptr) fx
    poke (ptr `plusPtr` sizeOf (undefined :: f x)) (rest :: HList f xs) 


(<:>) = Cons
infixr 8 <:>
