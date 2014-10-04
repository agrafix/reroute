{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-} -- for ReverseLoop type family
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.HVect
  ( HVect (..)
  , HVectElim
  , Append, hVectAppend
  , ReverseLoop, Reverse, hVectReverse
  , hVectUncurry
  , Rep (..), HasRep (..)
  , hVectCurryExpl, hVectCurry
  , packExpl, pack
  ) where

data HVect (ts :: [*]) where
  HNil :: HVect '[]
  HCons :: t -> HVect ts -> HVect (t ': ts)

-- todo: use a closed type family once GHC 7.6 compatibility is dropped
type family HVectElim (ts :: [*]) (a :: *) :: *
type instance HVectElim '[] a = a
type instance HVectElim (t ': ts) a = t -> HVectElim ts a

-- todo: use a closed type family once GHC 7.6 compatibility is dropped
type family Append (as :: [*]) (bs :: [*]) :: [*]
type instance Append '[] bs = bs
type instance Append (a ': as) bs = a ': (Append as bs)

hVectAppend :: HVect as -> HVect bs -> HVect (Append as bs)
hVectAppend HNil bs = bs
hVectAppend (HCons a as) bs = HCons a (hVectAppend as bs)

type family ReverseLoop (as :: [*]) (bs :: [*]) :: [*]
type instance ReverseLoop '[] bs = bs
type instance ReverseLoop (a ': as) bs = ReverseLoop as (a ': bs)

type Reverse as = ReverseLoop as '[]

hVectReverse :: HVect as -> HVect (Reverse as)
hVectReverse vs = go vs HNil
  where
    go :: HVect as -> HVect bs -> HVect (ReverseLoop as bs)
    go HNil bs = bs
    go (HCons a as) bs = go as (HCons a bs)

hVectUncurry :: HVectElim ts a -> HVect ts -> a
hVectUncurry f HNil = f
hVectUncurry f (HCons x xs) = hVectUncurry (f x) xs

data Rep (ts :: [*]) where
  RNil :: Rep '[]
  RCons :: Rep ts -> Rep (t ': ts)

class HasRep (ts :: [*]) where
  hasRep :: Rep ts

instance HasRep '[] where
  hasRep = RNil

instance HasRep ts => HasRep (t ': ts) where
  hasRep = RCons hasRep

hVectCurryExpl :: Rep ts -> (HVect ts -> a) -> HVectElim ts a
hVectCurryExpl RNil f = f HNil
hVectCurryExpl (RCons r) f = \x -> hVectCurryExpl r (f . HCons x)

hVectCurry :: HasRep ts => (HVect ts -> a) -> HVectElim ts a
hVectCurry = hVectCurryExpl hasRep

buildElim :: Rep ts -> (HVect ts -> HVect ss) -> HVectElim ts (HVect ss)
buildElim RNil f = f HNil
buildElim (RCons r) f = \x -> buildElim r (f . HCons x)

packExpl :: Rep ts -> (forall a. HVectElim ts a -> a) -> HVect ts
packExpl rep f = f (buildElim rep id)

pack :: HasRep ts => (forall a. HVectElim ts a -> a) -> HVect ts
pack = packExpl hasRep