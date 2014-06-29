{-# LANGUAGE LambdaCase, Rank2Types #-}

module Text.Taggy.Lens (
  name,
  attrs,
  children,
  htmlWith,
  html,
  element,
  content
) where

import Control.Lens (Lens', Prism', prism', (<&>), preview, ix)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import Text.Taggy (Element(..), Node(..), Renderable(..), domify, taggyWith)

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
-- with translation by default, see 'htmlWith.'
-- 
-- >>> (True, False) & both %~ \convert -> "<span>&hearts;</span>" ^? htmlWith convert . element . children . ix 0 . content
-- (Just "\9829", Just "&hearts;")
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
-- >>> markup ^? htmlWith True == markup ^? html
-- True

html :: Prism' Lazy.Text Node
html = htmlWith True

-- | A lens into the name of a given DOM element.

-- >>> markup ^? html . element . name
-- Just "html"
-- >>> markup & html . element . name .~ "sgml"
-- "<sgml><head><title>My Page</title></head><body><blink>Hello, world!</blink></body></sgml>"

name :: Lens' Element Text
name f el = f (eltName el) <&> \n -> el {eltName=n}

-- | A lens into the attributes of a given DOM element.
--
-- >>> let markup = "<html xmlns=\"http://www.w3.org/1999/xhtml\"><head></head><body></body></html>" :: Lazy.Text
-- >>> markup ^? html . element . attrs
-- Just fromList [("xmlns","http://www.w3.org/1999/xhtml")]
-- >>> markup ^? html . element . attrs . at "xmlns" & join
-- Just "http://www.w3.org/1999/xhtml
-- >>> markup ^? html . element . attrs . at "style" & join
-- Nothing
-- >>> markup & html . element . attrs . at "xmlns" ?~ "http://www.w3.org/TR/html4/"
-- "<html xmlns=\"http://www.w3.org/TR/html4/\"><title>My Page</title></head><body><blink>Hello, world!</blink></body></html>"

attrs :: Lens' Element (HashMap Text Text)
attrs f el = f (eltAttrs el) <&> \as -> el {eltAttrs=as}

-- | A lens into the child nodes of a given DOM element.
--
-- >>> let markup = "<html><title>Your title goes here.</title><body>Your content goes here.</body></html>" :: Lazy.Text
-- >>> markup ^? html . element . children . ix 0
-- Just (NodeElement (Element {eltName = "head", eltAttrs = fromList [], eltChildren = [NodeElement (Element {eltName = "title", eltAttrs = fromList [], eltChildren = [NodeContent "Your content goes here."]})]}))
-- >>> markup & html . element . children . ix 0 . element . children .~ [NodeContent "Lenses!"]
-- "<html><title>Lenses!</title><body>Your content goes here.</body></html>"

children :: Lens' Element [Node]
children f el = f (eltChildren el) <&> \cs -> el {eltChildren = cs}

-- | Construct a node from an element, or attempt to extract an element from a node.
--
-- >>> let markup = "<html><head><title>My Page</title></head><body><blink>Hello, world!</blink></body></html>" :: Lazy.Text
-- >>> markup ^? html . element
-- Just (Element {eltName = "html", eltAttrs = fromList [], eltChildren = [NodeElement (Element {eltName = "head", eltAttrs = fromList [], eltChildren = [NodeElement (Element {eltName = "title", eltAttrs = fromList [], eltChildren = [NodeContent "My Page"]})]}),NodeElement (Element {eltName = "body", eltAttrs = fromList [], eltChildren = [NodeElement (Element {eltName = "blink", eltAttrs = fromList [], eltChildren = [NodeContent "Hello, world!"]})]})]})
-- >>> markup ^? html . element ^. to fromJust . re element == markup ^? html
-- True

element :: Prism' Node Element
element =  prism' NodeElement $ \case { NodeElement e -> Just e; _ -> Nothing }

-- | Construct a node from text, or attempt to extract text from a node.
--
-- >>> let markup = "<foo>bar</foo>" :: Lazy.Text
-- >>> markup ^? html . element . children . traverse . content
-- Just "bar"
-- >>> markup & html . element . children . traverse . content .~ "baz"
-- "<foo>baz</foo>"

content :: Prism' Node Text
content = prism' NodeContent $ \case { NodeContent c -> Just c; _ -> Nothing }
