module Text.Taggy.Lens (
  eltName, 
  eltAttrs, 
  eltChildren
) where

import Control.Lens (Lens', (<&>))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Text.Taggy.DOM (Element, Node)
import qualified Text.Taggy.DOM as DOM (Element(..))

eltName :: Lens' Element Text
eltName f el = f (DOM.eltName el) <&> \name -> el {DOM.eltName=name}

eltAttrs :: Lens' Element (HashMap Text Text)
eltAttrs f el = f (DOM.eltAttrs el) <&> \attrs -> el {DOM.eltAttrs=attrs}

eltChildren :: Lens' Element [Node]
eltChildren f el = f (DOM.eltChildren el) <&> \children -> el {DOM.eltChildren = children}
