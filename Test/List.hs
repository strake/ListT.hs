{-# LANGUAGE TypeApplications #-}

module Test.List where

import Control.Applicative
import Data.Function (on)
import Data.Functor.Identity
import Test.SmallCheck
import Test.Tasty
import Test.Tasty.SmallCheck

import Control.Monad.Trans.List

tests :: TestTree
tests =
    testGroup "List"
    [testProperty "List = ListT Identity" $ changeDepth (min 4) $ \ (xs, ys) ->
     liftA2 (,) xs ys == (runIdentity . toListM $ (liftA2 (,) `on` fromList @_ @Int) xs ys),
     testProperty "toListM" $ (==) <*> runIdentity . toListM . fromList @_ @Int,
     testProperty "toReverseListM" $
     liftA2 (==) reverse $ runIdentity . toReverseListM . fromList @_ @Int]
