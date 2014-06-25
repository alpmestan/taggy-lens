{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.LensSpec (spec) where

import Control.Lens ((^.),(.~),at)
import Data.Monoid ((<>))
import Data.HashMap.Strict (fromList)
import Text.Taggy.Lens (name, attrs, children)
import Text.Taggy.DOM (domify, Element(..), Node(NodeElement))
import Text.Taggy.Parser (taggyWith)
import Test.Hspec (describe, it, shouldBe, Spec)
  
document :: Element  
document = (\(NodeElement e) -> e) . head . domify . taggyWith False $
  "<html xmlns=\"http://www.w3.org/1999/xhtml\"></html>"

spec :: Spec
spec = do
  describe "name" $ do
    it "Should get the name of a given element." $ do
      document ^. name `shouldBe` "html"
    it "Should set the name of the given element." $ do
      let document' = name .~ "sgml" $ document
      eltName document' `shouldBe` "sgml"
  describe "attrs" $ do
    it "Should get the attributes of a given element." $ do
      document ^. attrs ^. at "xmlns" `shouldBe` Just "http://www.w3.org/1999/xhtml"
      document ^. attrs ^. at "style" `shouldBe` Nothing
    it "Should set the attributes of a given element." $ do
      let attributes = eltAttrs document <> fromList [("style", "body { font-family: 'Comic Sans MS' }")]
          document'  = attrs .~ attributes $ document
      eltAttrs document' `shouldBe` attributes
  describe "children" $ do
    it "Should get child nodes of the given element." $ do
      document ^. children `shouldBe` []
    it "Should set the child nodes of a given element." $ do
      let elements  = domify $ taggyWith False "<a>bar</a>"
          document' = children .~ elements $ document
      eltChildren document' `shouldBe` elements
