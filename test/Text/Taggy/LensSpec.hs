{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.LensSpec (spec) where

import Control.Lens ((^.),(.~),at,(^?),re)
import Data.HashMap.Strict (fromList)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Text.Taggy.Lens (name, attrs, children, html)
import Text.Taggy.DOM (domify, Element(..), Node(NodeElement))
import Text.Taggy.Parser (taggyWith)
import Test.Hspec (describe, it, shouldBe, Spec)

markup :: Text
markup = "<html xmlns=\"http://www.w3.org/1999/xhtml\"></html>"

node :: Node -- unsafe (head)
node = head . domify $ taggyWith False markup
  
element :: Element -- unsafe (partial)
element = (\(NodeElement e) -> e) node

spec :: Spec
spec = do
  describe "html" $ do
    it "Should parse given Text into a single root node." $ do
      markup ^? html `shouldBe` Just node
    it "Should render a given root node into text." $ do
      node ^. re html `shouldBe` markup
  describe "name" $ do
    it "Should get the name of a given element." $ do
      element ^. name `shouldBe` "html"
    it "Should set the name of the given element." $ do
      let element' = name .~ "sgml" $ element
      eltName element' `shouldBe` "sgml"
  describe "attrs" $ do
    it "Should get the attributes of a given element." $ do
      element ^. attrs ^. at "xmlns" `shouldBe` Just "http://www.w3.org/1999/xhtml"
      element ^. attrs ^. at "style" `shouldBe` Nothing
    it "Should set the attributes of a given element." $ do
      let attributes = eltAttrs element <> fromList [("style", "body { font-family: 'Comic Sans MS' }")]
          element'  = attrs .~ attributes $ element
      eltAttrs element' `shouldBe` attributes
  describe "children" $ do
    it "Should get child nodes of the given element." $ do
      element ^. children `shouldBe` []
    it "Should set the child nodes of a given element." $ do
      let elements  = domify $ taggyWith False "<a>bar</a>"
          element' = children .~ elements $ element
      eltChildren element' `shouldBe` elements
