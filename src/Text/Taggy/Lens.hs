{-# LANGUAGE LambdaCase, Rank2Types #-}

module Text.Taggy.Lens (
  name,
  attrs,
  children,
  htmlWith,
  html
) where

import Control.Lens (Lens', Prism', prism', (<&>), (^?), ix)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import Text.Taggy (Element(..), Node, render, domify, taggyWith)

name :: Lens' Element Text
name f el = f (eltName el) <&> \n -> el {eltName=n}

attrs :: Lens' Element (HashMap Text Text)
attrs f el = f (eltAttrs el) <&> \as -> el {eltAttrs=as}

children :: Lens' Element [Node]
children f el = f (eltChildren el) <&> \cs -> el {eltChildren = cs}

-- Documents have a single root node, anyone who says otherwise is a liar.

htmlWith :: Bool -> Prism' Lazy.Text Node 
htmlWith convertEntities = prism' render parse
  where parse = (^? ix 0) . domify . taggyWith convertEntities

html :: Prism' Lazy.Text Node
html = htmlWith True
