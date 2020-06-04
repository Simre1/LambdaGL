module Data.SizedVector where

import Foreign.Storable
import Foreign.Ptr

data Empty a = Empty

data Vector f a where
  EmptyV :: Vector Empty a
  Vector :: a -> Vector f a -> Vector (Vector f) a

vectorToList :: Vector f a -> [a]
vectorToList (EmptyV) = []
vectorToList (Vector a as) = a : vectorToList as

type V0 = Vector Empty
type V1 = Vector V0
type V2 = Vector V1
type V3 = Vector V2
type V4 = Vector V3
type V5 = Vector V4
type V6 = Vector V5
type V7 = Vector V6
type V8 = Vector V7
type V9 = Vector V8
type V10 = Vector V9
type V11 = Vector V10
type V12 = Vector V11
type V13 = Vector V12
type V14 = Vector V13
type V15 = Vector V14
type V16 = Vector V15


pattern V0 = EmptyV
pattern V1 a = Vector a V0
pattern V2 a b = Vector a (V1 b)
pattern V3 a b c = Vector a (V2 b c)
pattern V4 a b c d = Vector a (V3 b c d)
pattern V5 a b c d e = Vector a (V4 b c d e)
pattern V6 a b c d e f = Vector a (V5 b c d e f)
pattern V7 a b c d e f g = Vector a (V6 b c d e f g)
pattern V8 a b c d e f g h = Vector a (V7 b c d e f g h)
pattern V9 a b c d e f g h i = Vector a (V8 b c d e f g h i)
pattern V10 a b c d e f g h i j = Vector a (V9 b c d e f g h i j)
pattern V11 a b c d e f g h i j k = Vector a (V10 b c d e f g h i j k)
pattern V12 a b c d e f g h i j k l = Vector a (V11 b c d e f g h j i k l)
pattern V13 a b c d e f g h i j k l m = Vector a (V12 b c d e f g h i j k l m)
pattern V14 a b c d e f g h i j k l m n = Vector a (V13 b c d e f g h i j k l m n)
pattern V15 a b c d e f g h i j k l m n o = Vector a (V14 b c d e f g h i j k l m n o)
pattern V16 a b c d e f g h i j k l m n o p = Vector a (V15 b c d e f g h i j k l m n o p)

instance Show a => Show (Vector f a) where
  show = show . vectorToList

instance Functor (Vector f) where
  fmap f (Vector a as) = Vector (f a) $ fmap f as
  fmap _ EmptyV = EmptyV

instance Applicative (Vector Empty) where
  pure _ = EmptyV
  _ <*> _ = EmptyV

instance Applicative (Vector f) => Applicative (Vector (Vector f)) where
  pure a = Vector a $ pure a
  (Vector a as) <*> (Vector b bs) = Vector (a b) (as <*> bs)

instance Foldable (Vector f) where
  foldr f acc (EmptyV) = acc
  foldr f acc (Vector a as) = f a $ foldr f acc as
  foldl f acc (EmptyV) = acc
  foldl f acc (Vector a as) = foldl f (f acc a) as

instance Traversable (Vector f) where
  sequenceA EmptyV = pure EmptyV
  sequenceA (Vector fa fas) = Vector <$> fa <*> sequenceA fas

instance Storable a => Storable (Vector Empty a) where
  sizeOf _ = 0
  alignment _ = 0
  peek _ = pure EmptyV
  poke _ _ = pure ()

instance (Storable (Vector f a), Storable a) => Storable (Vector (Vector f) a) where
  sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: Vector f a)
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    a <- peek (castPtr ptr)
    rest <- peek (castPtr $ ptr `plusPtr` sizeOf (undefined :: a))
    pure $ Vector a rest
  poke ptr (Vector a rest) = do
    poke (castPtr ptr) a
    poke (castPtr $ ptr `plusPtr` sizeOf (undefined :: a)) rest

concatenateVectors :: Vector f1 a -> Vector f2 a -> Vector (VConcatenate f1 f2) a
concatenateVectors EmptyV f2 = f2
concatenateVectors (Vector a f) f2 = Vector a $ concatenateVectors f f2

type family VConcatenate f1 f2 where
  VConcatenate Empty f2 = f2
  VConcatenate (Vector f) f2 = Vector (VConcatenate f f2)
