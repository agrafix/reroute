{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Routing.Specs.SafeRoutingSpec where

import Test.Hspec

import Control.Monad.Identity
import Web.Routing.SafeRouting
import Web.Routing.AbstractRouter
import qualified Data.Text as T

{-
     it "should capture variables in routes" $
          do matchRoute "/bar/5" routingTree `shouldBe` oneMatch (vMap [("baz", "5")]) [3]
             matchRoute "/bar/23/baz" routingTree `shouldBe` oneMatch (vMap [("baz", "23")]) [4]
             matchRoute "/bar/23/baz/100" routingTree `shouldBe` oneMatch (vMap [("baz", "23"), ("bim", "100")]) [4]
             matchRoute "/ba/23/100" routingTree `shouldBe` oneMatch (vMap [("baz", "23"), ("bim", "100")]) [4]
             matchRoute "/entry/344/2014-20-14T12:23" routingTree `shouldBe` oneMatch (vMap [("cid", "344"), ("since", "2014-20-14T12:23")]) [6]
             matchRoute "/entry/bytags/344/2014-20-14T12:23" routingTree `shouldBe` oneMatch (vMap [("cid", "344"), ("since", "2014-20-14T12:23")]) [7]
             matchRoute "/entry/2/rel/3" routingTree `shouldBe` oneMatch (vMap [("eid", "2"), ("cid", "3")]) [9]
       it "should handle multiple possibile matches correctly" $
          do matchRoute "/bar/bingo" routingTree `shouldBe` multiMatch
             matchRoute "/entry/1/audit" routingTree `shouldBe` multiMatch'
-}

data ReturnVar
   = IntVar Int
   | StrVar T.Text
   | BoolVar Bool
   | ListVar [ReturnVar]
   deriving (Show, Eq, Read)

spec :: Spec
spec =
    describe "SafeRouting Spec" $
    do it "shoud match known routes" $
          do checkRoute "" [StrVar "root"]
             checkRoute "/bar" [StrVar "bar"]
       it "shoudn't match unknown routes" $
          do checkRoute "/random" []
             checkRoute "/baz" []
       it "should capture variables in routes" $
          do checkRoute "/bar/23/baz" [IntVar 23]
             checkRoute "/bar/23/baz/100" [ListVar [IntVar 23, IntVar 100]]
             checkRoute "/bar/23/100" [ListVar [IntVar 23, IntVar 100]]
             checkRoute "/entry/344/2014-20-14T12:23" [ListVar [IntVar 344, StrVar "2014-20-14T12:23"]]
             checkRoute "/entry/bytags/344/2014-20-14T12:23" [ListVar [IntVar 344, StrVar "2014-20-14T12:23"]]
             checkRoute "/entry/2/rel/3"  [ListVar [IntVar 2, IntVar 3]]
       it "should handle multiple possible matches correctly" $
          do checkRoute "/bar/5" [IntVar 5, StrVar "5"]
             checkRoute "/bar/bingo" [StrVar "bar/bingo", StrVar "bingo"]
             checkRoute "/entry/1/audit" [IntVar 1,ListVar [IntVar 1,StrVar "audit"]]
    where
      checkRoute :: T.Text -> [ReturnVar] -> Expectation
      checkRoute r x =
          let matches = handleFun (filter (not . T.null) $ T.splitOn "/" r)
          in (map (runIdentity . snd) matches) `shouldBe` x
      regIf = typesafeRegistry

      handleFun :: [T.Text] -> [(ParamMap, Identity ReturnVar)]
      handleFun = handleFun' True
      (_, handleFun', _) =
          runIdentity (runRegistry regIf handleDefs)

      defR :: Path xs -> (HListElim xs (Identity ReturnVar))
           -> RegistryT Path (HListElim' (Identity ReturnVar))
              Identity ReturnVar (PathMap (Identity ReturnVar)) [ReturnVar] Bool Identity ()
      defR path action = hookRoute True path (HListElim' action)

      handleDefs :: RegistryT Path (HListElim' (Identity ReturnVar))
                    Identity ReturnVar (PathMap (Identity ReturnVar)) [ReturnVar] Bool Identity ()
      handleDefs =
          do defR root $ return (StrVar "root")
             defR "bar" $ return (StrVar "bar")
             defR ("bar" </> var) (return . IntVar)
             defR ("bar" </> var </> "baz") (return . IntVar)
             defR ("bar" </> var </> "baz" </> var) $ \i i2 ->
                 return (ListVar [IntVar i, IntVar i2])
             defR ("bar" </> var </> var) $ \i i2 ->
                 return (ListVar [IntVar i, IntVar i2])
             defR ("entry" </> var </> var) $ \i st ->
                 return (ListVar [IntVar i, StrVar st])
             defR ("entry" </> "bytags" </> var </> var) $ \i st ->
                 return (ListVar [IntVar i, StrVar st])
             defR ("entry" </> var </> "rel" </> var) $ \i i2 ->
                 return (ListVar [IntVar i, IntVar i2])
             defR ("bar" </> "bingo") $ return (StrVar "bar/bingo")
             defR ("bar" </> var) $ (return . StrVar . T.pack)
             defR ("entry" </> var </> "audit") (return . IntVar)
