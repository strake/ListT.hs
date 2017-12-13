module Main where

import Test.Tasty

import qualified Test.List as L

main :: IO ()
main = defaultMain L.tests
