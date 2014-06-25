{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.LensSpec (spec) where

import Control.Lens ((^.),(.~),at)
import Data.Monoid ((<>))
import Data.HashMap.Strict (fromList)
import Text.Taggy.Lens (eltName, eltAttrs, eltChildren)
import Text.Taggy.DOM (domify, Element, Node(NodeElement))
import qualified Text.Taggy.DOM as DOM (Element(..))
import Text.Taggy.Parser (taggyWith)
import Test.Hspec (describe, it, shouldBe, Spec)
  
document :: Element  
document = (\(NodeElement e) -> e) . head . domify . taggyWith False $
  "<html xmlns=\"http://www.w3.org/1999/xhtml\"></html>"

spec :: Spec
spec = do
  describe "eltName" $ do
    it "Should get the name of a given element." $ do
      document ^. eltName `shouldBe` "html"
    it "Should set the name of the given element." $ do
      let document' = eltName .~ "sgml" $ document
      DOM.eltName document' `shouldBe` "sgml"
  describe "eltAttrs" $ do
    it "Should get the attributes of a given element." $ do
      document ^. eltAttrs ^. at "xmlns" `shouldBe` Just "http://www.w3.org/1999/xhtml"
      document ^. eltAttrs ^. at "style" `shouldBe` Nothing
    it "Should set the attributes of a given element." $ do
      let attributes = DOM.eltAttrs document <> fromList [("style", "body { font-family: 'Comic Sans MS' }")]
          document'  = eltAttrs .~ attributes $ document
      DOM.eltAttrs document' `shouldBe` attributes
  describe "eltChildren" $ do
    it "Should get child nodes of the given element." $ do
      document ^. eltChildren `shouldBe` []
    it "Should set the child nodes of a given element." $ do
      let children  = domify $ taggyWith False "<a>bar</a>"
          document' = eltChildren .~ children $ document
      DOM.eltChildren document' `shouldBe` children
