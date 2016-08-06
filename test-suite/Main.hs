{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Data.Text.PieceTable
import           Paths_piece_table
import qualified Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
    test <- testSpec "piece-table" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = do
    it "new (from disk) works" $ do
      h <- getDataFileName "test-resources/hello.txt"
      t <- new h
      unsafeRender t `shouldBe` "hello with spaces\n"
    it "new' (from disk) works" $ do
      h <- getDataFileName "test-resources/hello.txt"
      t <- new' (Just $ newViewPort 2 8) h
      unsafeRender t `shouldBe` "llo with"
    it "newFromText works" $ do
      t <- newFromText "test test"
      unsafeRender t `shouldBe` "test test"
    it "newFromText' works" $ do
      t <- newFromText' (Just $ newViewPort 2 2) "hello"
      unsafeRender t `shouldBe` "ll"
    it "unsafeRender of an unsplitted text works" $ do
      t <- newFromText "hello with spaces"
      unsafeRender t `shouldBe` "hello with spaces"
    it "Adding a single char at the beginning works" $ do
      t <- newFromText "hello with spaces"
      unsafeRender (insert (C8.singleton 'a') 0 t) `shouldBe` "ahello with spaces"
    it "Multiple atomic inserts" $ do
      t <- newFromText ""
      let t1 = insert (C8.singleton 'a') 0 t
      let t2 = insert (C8.singleton 'b') 1 t1
      let t3 = insert (C8.singleton 'c') 2 t2
      unsafeRender t1 `shouldBe` "a"
      unsafeRender t2 `shouldBe` "ab"
      unsafeRender t3 `shouldBe` "abc"
    it "Adding a single char at the middle works" $ do
      t <- newFromText "hello o you"
      unsafeRender (insert (C8.singleton 't') 6 t) `shouldBe` "hello to you"
    it "Adding a single char at the end works" $ do
      t <- newFromText "hell"
      unsafeRender (insert (C8.singleton 'o') 4 t) `shouldBe` "hello"
    it "Adding multiple words works" $ do
      h <- getDataFileName "test-resources/hello.txt"
      t <- new h
      unsafeRender (insert (C8.pack "this is a nice world ") 6 t) `shouldBe` "hello this is a nice world with spaces\n"
    it "Deleting a single char works" $ do
      t <- newFromText "hello with spaces"
      unsafeRender (delete 0 t) `shouldBe` "ello with spaces"
