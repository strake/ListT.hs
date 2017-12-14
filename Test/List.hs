{-# LANGUAGE TypeApplications #-}

module Test.List where

import Control.Applicative
import Control.Arrow
import Data.Function (on)
import Data.Functor.Identity
import qualified Data.List as L
import Numeric.Natural
import Test.SmallCheck
import Test.Tasty
import Test.Tasty.SmallCheck

import Control.Monad.Trans.List

tests :: TestTree
tests =
    testGroup "List"
    [testProperty "List = ListT Identity" $ changeDepth (min 4) $ \ (xs, ys) ->
     liftA2 (,) xs ys == (runIdentity . toListM $ (liftA2 (,) `on` fromList @_ @_ @Int) xs ys),
     testProperty "toListM" $ (==) <*> runIdentity . toListM . fromList @_ @_ @Int,
     testProperty "toReverseListM" $
     liftA2 (==) reverse $ runIdentity . toReverseListM . fromList @_ @_ @Int,
     testProperty "splitAtM" $ (liftA2 . liftA2) (==) L.genericSplitAt $ \ n ->
     runIdentity . splitAtM (n :: Natural) . fromList @_ @_ @Int >>>
     id *** runIdentity . toListM]
