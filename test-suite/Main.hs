{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.PieceTable
import           Paths_piece_table
import qualified Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
    test <- testSpec "piece-table" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    it "new (from disk) works" $ do
      h <- getDataFileName "test-resources/hello.txt"
      t <- new h
      unsafeRender t `shouldBe` "hello with spaces\n"
    it "unsafeRender of an unsplitted text works" $ do
      t <- newFromText "hello with spaces"
      unsafeRender t `shouldBe` "hello with spaces"
    it "Adding a single char works" $ do
      t <- newFromText "hello with spaces"
      unsafeRender (insert 'a' 0 t) `shouldBe` "ahello with spaces"
    it "Deleting a single char works" $ do
      t <- newFromText "hello with spaces"
      unsafeRender (delete 0 t) `shouldBe` "ello with spaces"
