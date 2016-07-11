{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.PieceTable
import qualified Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
    test <- testSpec "piece-table" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    it "unsafeRender of an unsplitted text works" $ do
        unsafeRender (newFromText "hello with spaces") == "hello with spaces"
