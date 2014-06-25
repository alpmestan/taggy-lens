module Text.Taggy.Lens (
  name,
  attrs,
  children
) where

import Control.Lens (Lens', (<&>))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Text.Taggy.DOM (Element(..), Node)

name :: Lens' Element Text
name f el = f (eltName el) <&> \n -> el {eltName=n}

attrs :: Lens' Element (HashMap Text Text)
attrs f el = f (eltAttrs el) <&> \as -> el {eltAttrs=as}

children :: Lens' Element [Node]
children f el = f (eltChildren el) <&> \cs -> el {eltChildren = cs}
