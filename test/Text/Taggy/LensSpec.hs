{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.LensSpec (spec) where

import Prelude hiding (elem, length)
import Control.Lens ((^.),(.~),at,(^?),re,_Just,(&),(?~),to,folded,(^..),only)
import Data.HashMap.Strict (fromList)
import Data.Monoid ((<>))
import Data.Text (isSuffixOf, length)
import Data.Text.Lazy (Text)
import Text.Taggy.Lens (name, attrs, children, html, htmlWith, element, content, attr, attributed, named, contents, elements)
import Text.Taggy.DOM (domify, Element(..), Node(..))
import Text.Taggy.Parser (taggyWith)
import Test.Hspec (describe, it, shouldBe, Spec)

markup :: Text
markup = "<html xmlns=\"http://www.w3.org/1999/xhtml\"></html>"

node :: Node -- unsafe (head)
node = head . domify $ taggyWith False markup
  
elem :: Element -- unsafe (partial)
elem = (\(NodeElement e) -> e) node

spec :: Spec
spec = do
  describe "html" $ do
    it "Should parse given Text into a single root node." $ do
      markup ^? html `shouldBe` Just node
    it "Should render a given root node into text." $ do
      node ^. re html `shouldBe` markup
    it "Should escape HTML entities in rendering." $ do
      "&" ^? html ^. _Just . re html `shouldBe` "&amp;"
  describe "htmlWith" $ do
    it "When parametrised with False, should not escape HTML entities in parsing." $ do
      "&" ^? htmlWith False `shouldBe` Just (NodeContent "&")
    it "When parametrised with False, should not escape HTML entities in rendering." $ do
      "&" ^? htmlWith False ^. _Just . re (htmlWith False) `shouldBe` "&"
  describe "name" $ do
    it "Should get the name of a given element." $ do
      elem ^. name `shouldBe` "html"
    it "Should set the name of the given element." $ do
      let element' = name .~ "sgml" $ elem
      eltName element' `shouldBe` "sgml"
  describe "named" $ do
    it "Should traverse only elements who's name matches a specific property." $ do
      let markup' = "<html><foo>bar</foo><baz>qux</baz><quux>corge</quux></html>"
      markup' ^.. htmlWith False . element . elements . named (to length . only 3) . name `shouldBe` ["foo", "baz"]
  describe "attrs" $ do
    it "Should get the attributes of a given element." $ do
      elem ^. attrs ^. at "xmlns" `shouldBe` Just "http://www.w3.org/1999/xhtml"
      elem ^. attrs ^. at "style" `shouldBe` Nothing
    it "Should set the attributes of a given element." $ do
      let attributes = eltAttrs elem <> fromList [("style", "body { font-family: 'Comic Sans MS' }")]
          element'   = attrs .~ attributes $ elem
      eltAttrs element' `shouldBe` attributes
  describe "attr" $ do
    it "Should get the value of a named attribute." $ do
      elem ^. attr "xmlns" `shouldBe` Just "http://www.w3.org/1999/xhtml"
      elem ^. attr "style" `shouldBe` Nothing
    it "Should set the value of a named attribute." $ do
      let style    = "body { font-family: 'Comic Sans MS' }"
          element' = elem & attr "style" ?~ style
      element' ^. attr "style" `shouldBe` Just style
  describe "attributed" $ do
    it "Should traverse only attributes satisfying a given property." $ do
      elem ^? attributed (folded . to (isSuffixOf "xhtml")) `shouldBe` Just elem
  describe "children" $ do
    it "Should get child nodes of the given element." $ do
      elem ^. children `shouldBe` []
    it "Should set the child nodes of a given element." $ do
      let elements  = domify $ taggyWith False "<a>bar</a>"
          element' = children .~ elements $ elem
      eltChildren element' `shouldBe` elements
  let text = "The quick brown fox jumps over the lazy dog."
  describe "element" $ do
    it "Should lift a given Element into a Node" $ do
      elem ^. re element `shouldBe` node
    it "Should try to extract an Element from a given Node." $ do
      node ^? element `shouldBe` Just elem
      NodeContent text ^? element `shouldBe` Nothing
  describe "elements" $ do
    it "Should traverse the immediate children of an element that are also elements." $ do
      let markup' = "<html><foo></foo><bar></bar><baz></baz></html>"
      markup' ^.. html . element . elements . name `shouldBe` ["foo", "bar", "baz"]
  describe "content" $ do
    it "Should lift provided Text into a Node." $ do
      text ^. re content `shouldBe` NodeContent text
    it "Should try to extract Text from a given node." $ do
      node ^? content `shouldBe` Nothing
      NodeContent text ^? content `shouldBe` Just text
  describe "contents" $ do
    it "Should traverse the immediate children of an element that are text nodes." $ do
      let markup' = "<html><foo></foo>bar<baz></baz>qux</html>" 
      markup' ^.. html . element . contents `shouldBe` ["bar", "qux"]
