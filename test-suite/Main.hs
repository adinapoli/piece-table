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
    it "new' (from disk) works" $ do
      h <- getDataFileName "test-resources/hello.txt"
      t <- new' (Just $ newViewPort 1 5) h
      unsafeRender t `shouldBe` "ello "
    it "newFromText works" $ do
      t <- newFromText "άλφα"
      unsafeRender t `shouldBe` "άλφα"
    it "newFromText' works" $ do
      t <- newFromText' (Just $ newViewPort 2 2) "hello"
      unsafeRender t `shouldBe` "ll"
    it "unsafeRender of an unsplitted text works" $ do
      t <- newFromText "hello with spaces"
      unsafeRender t `shouldBe` "hello with spaces"
    it "Adding a single char at the beginning works" $ do
      t <- newFromText "hello with spaces"
      unsafeRender (insert 'a' 0 t) `shouldBe` "ahello with spaces"
    it "Multiple atomic inserts" $ do
      t <- newFromText ""
      let t1 = insert 'a' 0 t
      let t2 = insert 'b' 1 t1
      let t3 = insert 'c' 2 t2
      unsafeRender t1 `shouldBe` "a"
      unsafeRender t2 `shouldBe` "ab"
      unsafeRender t3 `shouldBe` "abc"
    it "Adding a single char at the middle works" $ do
      t <- newFromText "hello o you"
      unsafeRender (insert 't' 6 t) `shouldBe` "hello to you"
    it "Adding a single char at the end works" $ do
      t <- newFromText "hell"
      unsafeRender (insert 'o' 4 t) `shouldBe` "hello"
    it "Deleting a single char works" $ do
      t <- newFromText "hello with spaces"
      unsafeRender (delete 0 t) `shouldBe` "ello with spaces"
