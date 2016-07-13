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
        unsafeRender (newFromText "hello with spaces") `shouldBe` "hello with spaces"
    it "Adding a single char works" $ do
        unsafeRender (insert 'a' 0 $ newFromText "hello with spaces") `shouldBe` "ahello with spaces"
    it "Deleting a single char works" $ do
        unsafeRender (delete 0 $ newFromText "hello with spaces") `shouldBe` "ello with spaces"
