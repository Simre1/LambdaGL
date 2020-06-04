module Graphics.LambdaGL.Types.List where

import GHC.TypeLits
import Data.Proxy

type family MustContain x (a :: [*]) where
  MustContain x (x:_) = 'True
  MustContain x (y:ys) = MustContain x ys
  MustContain _ '[] = False

type family ReturnUnique a (as :: [*]) where
  ReturnUnique a '[] = Just a
  ReturnUnique a (a:xs) = Nothing
  ReturnUnique a (x:xs) = ReturnUnique a xs

type family AddJust (a :: Maybe *) (b :: [*]) where
  AddJust 'Nothing b = b
  AddJust ('Just a) b = a:b

type family Unique (a :: [*]) where
  Unique '[] = '[]
  Unique (a:as) = AddJust (ReturnUnique a as) (Unique as)

type family Last (a :: [*]) where
  Last '[a] = a
  Last (a:b:r) = Last (b:r)

type family Init (a :: [*]) where
  Init '[] = '[]
  Init '[a] = '[]
  Init (a:b:r) = a : Init (b:r)

type family ReplaceAt (a :: [*]) (i::Nat) x where
  ReplaceAt (a:as) 0 x = x:as
  ReplaceAt (a:as) i x = ReplaceAt (as) (i-1) x

type family AtIndex (a :: [*]) (i :: Nat) where
  AtIndex (a:as) 0 = a
  AtIndex (a:as) i = AtIndex as (i-1)


type family Take (x :: Nat) (a :: [*]) where
  Take 0 '[] = '[]
  Take x (a:as) = a:Take (x-1) as

type family And (a :: Bool) (b :: Bool) where
  And 'True 'True = 'True
  And _ _ = 'False

type family Contains (a :: [*]) (b :: [*]) where
  Contains u u = 'True
  Contains '[] b = 'True
  Contains a '[] = 'False
  Contains (a:as) b = And (MustContain a b) (Contains as b)

type family Expand (a :: [*]) (b :: [*]) where
  Expand '[] '[] = '[]
  Expand '[] (b:bs) = b : Expand '[] bs
  Expand (a:as) '[] = a : Expand as '[]
  Expand (a:as) (a:bs) = a : Expand as bs

type EqualElements a b = And (Contains a b) (Contains b a)

type family Merge (xs :: [*]) (ys :: [*]) where
  Merge '[] ys = ys
  Merge (x:xs) ys = x:Merge xs ys

type family Append (xs :: [*]) x where
  Append '[] y = '[y]
  Append (x:xs) y = x : Append xs y 

type family AppendMerge (xs :: [*]) (ys :: [*]) where
  AppendMerge xs '[] = xs
  AppendMerge (xs) (y:ys) = AppendMerge (Append (xs) y) ys


class CountElements (i :: [*]) where
  countElements :: Proxy i -> Int

instance CountElements '[] where
  countElements _ = 0

instance CountElements as => CountElements (a : as) where
  countElements _ = 1 + countElements (Proxy @as)
