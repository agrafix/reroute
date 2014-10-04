{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.Routing.SafeRouting where

import qualified Data.PolyMap as PM
import Web.Routing.AbstractRouter

import Data.HList
import Data.Maybe
import Data.Monoid
import Data.String
import Web.PathPieces
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data RouteHandle m a
   = forall as. RouteHandle (Path as) (HListElim as (m a))

newtype HListElim' x ts = HListElim' { flipHListElim :: HListElim ts x }

data SafeRouter (m :: * -> *) a = SafeRouter

instance AbstractRouter (SafeRouter m a) where
    newtype Registry (SafeRouter m a) = SafeRouterReg (PathMap (m a))
    newtype RoutePath (SafeRouter m a) xs = SafeRouterPath (Path xs)
    type RouteAction (SafeRouter m a) = HListElim' (m a)
    type RouteAppliedAction (SafeRouter m a) = m a
    subcompCombine (SafeRouterPath p1) (SafeRouterPath p2) =
        SafeRouterPath $
        p1 </> p2
    emptyRegistry = SafeRouterReg emptyPathMap
    rootPath = SafeRouterPath Empty
    defRoute (SafeRouterPath path) action (SafeRouterReg m) =
        SafeRouterReg $
        insertPathMap (RouteHandle path (flipHListElim action)) m
    matchRoute (SafeRouterReg m) pathPieces =
        let matches = match m pathPieces
        in zip (replicate (length matches) HM.empty) matches

type family HListElim (ts :: [*]) (a :: *) :: *
type instance HListElim '[] a = a
type instance HListElim (t ': ts) a = t -> HListElim ts a

hListUncurry :: HListElim ts a -> HList ts -> a
hListUncurry f HNil = f
hListUncurry f (HCons x xs) = hListUncurry (f x) xs

data Path (as :: [*]) where
  Empty :: Path '[] -- the empty path
  StaticCons :: T.Text -> Path as -> Path as -- append a static path piece to path
  VarCons :: (PathPiece a, Typeable a) => Path as -> Path (a ': as) -- append a param to path

data PathMap x =
  PathMap
  { pm_here :: [x]
  , pm_staticMap :: HM.HashMap T.Text (PathMap x)
  , pm_polyMap :: PM.PolyMap PathPiece PathMap x
  }

instance Functor PathMap where
  fmap f (PathMap h s p) = PathMap (fmap f h) (fmap (fmap f) s) (fmap f p)

emptyPathMap :: PathMap x
emptyPathMap = PathMap mempty mempty PM.empty

instance Monoid (PathMap x) where
  mempty = emptyPathMap
  mappend (PathMap h1 s1 p1) (PathMap h2 s2 p2) =
    PathMap (h1 `mappend` h2) (HM.unionWith mappend s1 s2) (PM.unionWith mappend p1 p2)

insertPathMap' :: Path ts -> (HList ts -> x) -> PathMap x -> PathMap x
insertPathMap' path action (PathMap h s p) =
  case path of
    Empty -> PathMap (action HNil : h) s p
    StaticCons pathPiece path' ->
      let subPathMap = fromMaybe emptyPathMap (HM.lookup pathPiece s)
      in PathMap h (HM.insert pathPiece (insertPathMap' path' action subPathMap) s) p
    VarCons path' ->
      let alterFn = Just . insertPathMap' path' (\vs v -> action (HCons v vs))
                         . fromMaybe emptyPathMap
      in PathMap h s (PM.alter alterFn p)

insertPathMap :: RouteHandle m a -> PathMap (m a) -> PathMap (m a)
insertPathMap (RouteHandle path action) = insertPathMap' path (hListUncurry action)

match :: PathMap x -> [T.Text] -> [x]
match (PathMap h _ _) [] = h
match (PathMap _ s p) (pp:pps) =
  let staticMatches = maybeToList (HM.lookup pp s) >>= flip match pps
      varMatches = PM.lookupConcat (fromPathPiece pp)
                     (\piece pathMap' -> fmap ($ piece) (match pathMap' pps)) p
  in staticMatches ++ varMatches

-- | A route parameter
var :: (Typeable a, PathPiece a) => Path (a ': '[])
var = VarCons Empty

type Var a = Path (a ': '[])

-- | A static route piece
static :: String -> Path '[]
static s = StaticCons (T.pack s) Empty

instance (a ~ '[]) => IsString (Path a) where
    fromString = static

-- | The root of a path piece. Use to define a handler for "/"
root :: Path '[]
root = Empty

(</>) :: Path as -> Path bs -> Path (HAppendList as bs)
(</>) Empty xs = xs
(</>) (StaticCons pathPiece xs) ys = (StaticCons pathPiece (xs </> ys))
(</>) (VarCons xs) ys = (VarCons (xs </> ys))

renderRoute :: Path as -> HList as -> T.Text
renderRoute p h =
    T.intercalate "/" $ renderRoute' p h

renderRoute' :: Path as -> HList as -> [T.Text]
renderRoute' Empty _ = []
renderRoute' (StaticCons pathPiece pathXs) paramXs =
    ( pathPiece : renderRoute' pathXs paramXs )
renderRoute' (VarCons pathXs) (HCons val paramXs) =
    ( toPathPiece val : renderRoute' pathXs paramXs)
renderRoute' _ _ =
    error "This will never happen."

parse :: Path as -> [T.Text] -> Maybe (HList as)
parse Empty [] = Just HNil
parse _ [] = Nothing
parse path (pathComp : xs) =
    case path of
      Empty -> Nothing
      StaticCons pathPiece pathXs ->
          if pathPiece == pathComp
          then parse pathXs xs
          else Nothing
      VarCons pathXs ->
          case fromPathPiece pathComp of
            Nothing -> Nothing
            Just val ->
                let finish = parse pathXs xs
                in fmap (\parsedXs -> HCons val parsedXs) finish
