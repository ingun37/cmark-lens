{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CMark
import CMark.Lens
import Control.Lens
import Data.Text.IO qualified as TIO
import Test.Hspec

main :: IO ()
main = do
  content <- TIO.readFile "test/test.md"
  let node = commonmarkToNode [] content
  hspec $ do
    describe "Prelude.head" $ do
      it "returns the first element of a list" $ do
        node ^. _nodeType `shouldBe` DOCUMENT
        let nodes1 = node ^.. _nodesTraversal
        let nodes2 = node ^. _nodesLens
        let invalidText = node ^. _nodeType . _TEXT
        invalidText `shouldBe` ""
        nodes1 `shouldBe` nodes2
        let headingLevel = (node ^.. _nodesTraversal) ^? ix 0 . _nodeType . _HEADING
        headingLevel `shouldBe` Just 1
        let paragraph = node ^? _nodesLens . ix 1 . _nodesLens . ix 0 . _nodeType . _TEXT
        paragraph `shouldBe` Just "paragraph 1"
        let node' = over (_nodesTraversal . _nodesTraversal . _nodeType . _TEXT) (<> "?") node
        let paragraph' = node' ^? _nodesLens . ix 1 . _nodesLens . ix 0 . _nodeType . _TEXT
        paragraph' `shouldBe` Just "paragraph 1?"
        let node'' = set (_nodesTraversal . _nodesTraversal . _nodeType . _TEXT) "" node
        let paragraph'' = node'' ^? _nodesLens . ix 1 . _nodesLens . ix 0 . _nodeType . _TEXT
        paragraph'' `shouldBe` Just ""
