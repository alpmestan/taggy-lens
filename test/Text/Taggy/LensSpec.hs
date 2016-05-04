{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.LensSpec (spec) where

import           Control.Lens        (at, folded, ix, only, re, to, traverse,
                                      universe, (&), (.~), (?~), (^.), (^..),
                                      (^?), _Just)
import           Data.HashMap.Strict (fromList)
import           Data.Monoid         ((<>))
import           Data.Text           (isSuffixOf, length)
import           Data.Text.Lazy      (Text)
import           Prelude             hiding (elem, length)
import           Test.Hspec          (Spec, describe, it, shouldBe,
                                      shouldSatisfy)
import           Text.Taggy.DOM      (Element (..), Node (..), domify)
import           Text.Taggy.Lens     (allAttributed, allNamed, attr, attributed,
                                      attrs, children, content, contents,
                                      element, elements, html, htmlWith, name,
                                      named)
import           Text.Taggy.Parser   (taggyWith)

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
      markup' ^.. html . elements . named (to length . only 3) . name `shouldBe` ["foo", "baz"]
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
      "<html><bar>qux</bar></html>" ^.. html . element . children . traverse . element . name `shouldBe` ["bar"]
    it "Should set the child nodes of a given element." $ do
      let subtree = taggyWith False "<a>bar</a>" & domify
      eltChildren (elem & children .~ subtree) `shouldBe` subtree
  let text = "The quick brown fox jumps over the lazy dog."
  describe "element" $ do
    it "Should lift a given Element into a Node" $ do
      elem ^. re element `shouldBe` node
    it "Should try to extract an Element from a given Node." $ do
      node ^? element `shouldBe` Just elem
      NodeContent text ^? element `shouldBe` Nothing
  describe "elements" $ do
    it "Should traverse the immediate children of an element that are also elements directly." $ do
      let markup' = "<html><foo></foo><bar></bar><baz></baz></html>"
      markup' ^.. html . element . elements . name `shouldBe` ["foo", "bar", "baz"]
    it "Should traverse the immediate children of an element that are also elements, via a node." $ do
      let markup' = "<html><foo></foo><bar></bar><baz></baz></html>"
      markup' ^.. html . elements . name `shouldBe` ["foo", "bar", "baz"]
  describe "content" $ do
    it "Should lift provided Text into a Node." $ do
      text ^. re content `shouldBe` NodeContent text
    it "Should try to extract Text from a given node." $ do
      node ^? content `shouldBe` Nothing
      NodeContent text ^? content `shouldBe` Just text
  describe "contents" $ do
    it "Should traverse the immediate children of an element that are text nodes directly." $ do
      let markup' = "<html><foo></foo>bar<baz></baz>qux</html>"
      markup' ^.. html . element . contents `shouldBe` ["bar", "qux"]
    it "Should traverse the immediate children of an element that are text nodes, via a Node." $ do
      let markup' = "<html><foo></foo>bar<baz></baz>qux</html>"
      markup' ^.. html . contents `shouldBe` ["bar", "qux"]
  describe "validation of plated instance for node" $ do
    let markup' = "<html><foo>foo</foo>bar<baz></baz>qux</html>"
    it "Should be able to retrieve all transitive descendants of a given node." $ do
      markup' ^.. html . to universe . traverse . content `shouldBe` ["foo","bar","qux"]
  describe "validation of plated instance for element" $ do
    let markup' = "<html><foo></foo>bar<baz></baz>qux</html>"
    it "Should be able to retrieve all transitive descendants of a given node." $ do
      markup' ^.. html . element . to universe . traverse . name `shouldBe` ["html","foo","baz"]
  describe "allNamed" $ do
    let markup' = "<html><foo>bar<qux><foo class=\"withBaz\">baz</foo></qux></foo></html>"
    it "Should retrieve all nodes in a subtree who's name match a given predicate." $ do
      markup' ^.. html . allNamed (only "foo") . contents `shouldBe` ["bar", "baz"]
    it "Should compose with other folds in a way that they filter on the parent, not its children." $ do
      markup' ^.. html . allNamed (only "foo") . attributed (ix "class" . only "withBaz") . contents `shouldBe` ["baz"]
    it "Provided a node, should not exclude the parent element in its traversal." $ do
      markup  ^.. html . allNamed (only "html") `shouldSatisfy` not . null
    it "Provided an element, should not exclude this in its traversal." $ do
      markup ^.. html . element . allNamed (only "html") `shouldSatisfy` not . null
  describe "allAttributed" $ do
    let markup' = "<html><foo class=\"woah\">bar<qux class=\"woah\"></qux></foo><quux class=\"woah\"></quux></html>"
    it "Should retrieve all nodes in a subtree who's attributes match a given predicate." $ do
      markup' ^.. html . allAttributed (folded . only "woah") . name `shouldBe` ["foo", "qux", "quux"]
    it "Should compose with other folds in a way that they filter on the parent, not its children." $ do
      markup' ^.. html . allAttributed (ix "class" . only "woah") . named (only "foo") . name `shouldBe` ["foo"]
