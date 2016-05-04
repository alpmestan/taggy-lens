{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Text.Taggy.Lens (
  Node(..),
  Element(..),
  name,
  attrs,
  children,
  htmlWith,
  html,
  HasElement(..),
  content,
  attr,
  attributed,
  named,
  HasElements(..),
  HasContent(..),
  allNamed,
  allAttributed
) where

import           Control.Lens        (Fold, Lens', Plated (..), Prism',
                                      Traversal', at, filtered, has, ix,
                                      preview, prism', to, traverse, universe,
                                      (<&>))
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import qualified Data.Text.Lazy      as Lazy (Text)
import           Text.Taggy          (Element (..), Node (..), Renderable (..),
                                      domify, taggyWith)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens hiding (element, elements, children)
-- >>> import qualified Data.Text.Lazy as Lazy (Text)
-- >>> import Data.Monoid
-- >>> import Data.Text as T
-- >>> import Control.Monad (join)
-- >>> import Data.Maybe

-- | HTML document parsing and rendering.
--
-- >>> let markup = "<html><head><title>My Page</title></head><body><blink>Hello, world!</blink></body></html>" :: Lazy.Text
-- >>> markup ^? htmlWith False
-- Just (NodeElement (Element {eltName = "html", eltAttrs = fromList [], eltChildren = [NodeElement (Element {eltName = "head", eltAttrs = fromList [], eltChildren = [NodeElement (Element {eltName = "title", eltAttrs = fromList [], eltChildren = [NodeContent "My Page"]})]}),NodeElement (Element {eltName = "body", eltAttrs = fromList [], eltChildren = [NodeElement (Element {eltName = "blink", eltAttrs = fromList [], eltChildren = [NodeContent "Hello, world!"]})]})]}))
-- >>> (markup ^? htmlWith False) ^. _Just . re (htmlWith False) == markup
-- True
--
-- The provided boolean specifies whether named entities should be
-- translated to unicode. For a less general version of this prism,
-- with translation by default, see 'html.'
--
-- >>> (True, False) & both %~ \convert -> "<span>&hearts;</span>" ^? htmlWith convert . element . contents
-- (Just "\9829",Just "&hearts;")
--
-- The parser produces a single node; if markup describes more than one element at
-- the top-level, all but the first are discarded.
--
-- >>> (markup <> markup) ^? htmlWith False == markup ^? htmlWith False
-- True

htmlWith :: Bool -> Prism' Lazy.Text Node
htmlWith convertEntities = prism' (renderWith convertEntities)  parse
  where parse = preview (ix 0) . domify . taggyWith convertEntities

-- | Like 'htmlWith', but converts named entities by default.
--
-- >>> let markup = "<html><head><title>My Page</title></head><body><blink>Hello, world!</blink></body></html>" :: Lazy.Text
-- >>> markup ^? htmlWith True == markup ^? html
-- True

html :: Prism' Lazy.Text Node
html = htmlWith True

-- | A lens into the name of a given DOM element.

-- >>> markup ^? html . element . name
-- Just "html"
-- >>> markup & html . element . name .~ "sgml"
-- "<sgml><head><title>My Page</title></head><body><blink>Hello, world!</blink></body></sgml>"
-- >>> markup ^.. html . elements . name
-- ["head", "body"]

name :: Lens' Element Text
name f el = f (eltName el) <&> \n -> el {eltName=n}

-- | A lens into the attributes of a given DOM element.
--
-- >>> let markup = "<html xmlns=\"http://www.w3.org/1999/xhtml\"><head></head><body></body></html>" :: Lazy.Text
-- >>> markup ^? html . element . attrs
-- Just (fromList [("xmlns","http://www.w3.org/1999/xhtml")])
-- >>> markup ^? html . element . attrs . at "xmlns" & join
-- Just "http://www.w3.org/1999/xhtml"
-- >>> markup ^? html . element . attrs . at "style" & join
-- Nothing
-- >>> markup & html . element . attrs . at "xmlns" ?~ "http://www.w3.org/TR/html4/"
-- "<html xmlns=\"http://www.w3.org/TR/html4/\"><head></head><body></body></html>"

attrs :: Lens' Element (HashMap Text [Text])
attrs f el = f (eltAttrs el) <&> \as -> el {eltAttrs=as}

-- | Given an attribute name, a lens into its value for a given element.
--
-- >>> let markup = "<html><foo class=\"a\"></foo><bar class=\"b\"></bar></html>" :: Lazy.Text
-- >>> markup ^.. htmlWith False . elements . attr "class" . _Just
-- ["a","b"]

attr :: Text -> Lens' Element (Maybe [Text])
attr = fmap attrs . at

-- | A traversal into attributes matching a provided property.
--
-- >>> let markup = "<html><foo class=\"a\"></foo><bar class=\"a\"></bar></html>" :: Lazy.Text
-- >>> markup ^.. htmlWith False . elements . attributed (ix "class" . only "a") . name
-- ["foo","bar"]

attributed :: Fold (HashMap Text [Text]) a -> Traversal' Element Element
attributed prop = filtered . has $ attrs . prop

-- | A lens into the child nodes, elements, or contents of a given DOM element.
--
-- >>> let markup = "<html><title>Your title goes here.</title><body>Your content goes here.</body></html>" :: Lazy.Text
-- >>> markup ^? html . element . children . ix 0
-- Just (NodeElement (Element {eltName = "title", eltAttrs = fromList [], eltChildren = [NodeContent "Your title goes here."]}))
-- >>> markup & html . element . children . ix 0 . element . children .~ [NodeContent "Lenses!"]
-- "<html><title>Lenses!</title><body>Your content goes here.</body></html>"

children :: Lens' Element [Node]
children f el = f (eltChildren el) <&> \cs -> el {eltChildren = cs}

-- | A traversal into elements with a name matching a provided property.
--
-- >>> let markup = "<html><foo>bar</foo><baz>qux</baz><quux>corge</quux></html>" :: Lazy.Text
-- >>> markup ^.. htmlWith False . elements . named (to T.length . only 3) . name
-- ["foo","baz"]

named :: Fold Text a -> Traversal' Element Element
named prop = filtered . has $ name . prop

-- | Construct a node from an element, or attempt to extract an element from a node.
--
-- >>> let markup = "<html><head><title>My Page</title></head><body><blink>Hello, world!</blink></body></html>" :: Lazy.Text
-- >>> markup ^? html . element
-- Just (Element {eltName = "html", eltAttrs = fromList [], eltChildren = [NodeElement (Element {eltName = "head", eltAttrs = fromList [], eltChildren = [NodeElement (Element {eltName = "title", eltAttrs = fromList [], eltChildren = [NodeContent "My Page"]})]}),NodeElement (Element {eltName = "body", eltAttrs = fromList [], eltChildren = [NodeElement (Element {eltName = "blink", eltAttrs = fromList [], eltChildren = [NodeContent "Hello, world!"]})]})]})
-- >>> markup ^? html . element. re element == markup ^? html
-- True

class HasElement a where
  element :: Prism' a Element

instance HasElement Node where
  element = prism' NodeElement $ \case { NodeElement e -> Just e; _ -> Nothing }

instance HasElement Element where -- Iso
  element = prism' id (Just . id)

-- | A traversal into the immediate children of an element that are also elements, directly or via a Node.
--
-- >>> let markup = "<html><foo></foo><bar></bar><baz></baz></html>" :: Lazy.Text
-- >>> markup ^.. html . element . elements . name
-- ["foo","bar","baz"]
-- >>> markup ^.. html . elements . element . name
-- ["foo","bar","baz"]

class HasElements a where
  elements :: Traversal' a Element

instance HasElements Element where
  elements = children . traverse . element

instance HasElements Node where
  elements = element . elements

-- | Construct a node from text, or attempt to extract text from a node.
--
-- >>> let markup = "<foo>bar</foo>" :: Lazy.Text
-- >>> markup ^? html . element . children . traverse . content
-- Just "bar"
-- >>> markup & html . element . children . traverse . content .~ "baz"
-- "<foo>baz</foo>"

content :: Prism' Node Text
content = prism' NodeContent $ \case { NodeContent c -> Just c; _ -> Nothing }

-- | A traversal into the immediate children of an element that are text content, directly or via a Node.
--
-- >>> let markup = "<html><foo></foo>bar<baz></baz>qux</html>" :: Lazy.Text
-- >>> markup ^.. html . element . contents
-- ["bar","qux"]
-- >>> markup ^.. html . contents
-- ["bar","qux"]

class HasContent a where
  contents :: Traversal' a Text

instance HasContent Element where
  contents = children . traverse . content

instance HasContent Node where
  contents = element . contents

-- | Plated instances are available for Element and Node, such that we can retrieve all of their transitive descendants.
--
-- >>> let markup' = "<html><foo>foo</foo>bar<baz></baz>qux</html>" :: Lazy.Text
-- >>> markup' ^.. html . to universe . traverse . content
-- ["foo","bar","qux"]

instance Plated Node where
  plate = element . children . traverse

instance Plated Element where
  plate = elements

-- | A fold into all elements (current and descendants) who's name satisfy a provided property.
--
-- >>> let markup' = "<html><foo class=\"woah\">bar<qux><foo>baz</foo></qux></foo></html>" :: Lazy.Text
-- >>> markup' ^.. html . allNamed (only "foo") . contents
-- ["bar","baz"]
-- >>> markup' ^.. html . allNamed (only "foo") . attributed (ix "class" . only "woah") . contents
-- ["bar"]

allNamed :: HasElement a => Fold Text b -> Fold a Element
allNamed prop = element . to universe . traverse . named prop

-- | A fold into all elements (current and descendants) who's attributes satisfy a provided property.
--
-- >>> let markup' = "<html><foo class=\"woah\">bar<qux class=\"woah\"></qux></foo><quux class=\"woah\"></quux></html>" :: Lazy.Text
-- >>> markup' ^.. html . allAttributed (folded . only "woah") . name
-- ["foo","qux","quux"]
-- >>> markup' ^.. html . allAttributed (folded . only "woah") . named (only "foo") . name
-- ["foo"]

allAttributed :: HasElement a => Fold (HashMap Text [Text]) b -> Fold a Element
allAttributed prop = element . to universe . traverse . attributed prop
