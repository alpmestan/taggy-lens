{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens (to, only,(^.),ix, toListOf)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Network.HTTP.Client (Response)
import Network.Wreq (responseBody, get)
import Text.Taggy (Node)
import Text.Taggy.Lens (html, elements, named, children, contents)

data Upload = Upload Text Text Text deriving (Show)

table :: [Node] -> Upload
table row = do
  let date'    = row ^. ix 0 . contents 
      author'  = row ^. ix 1 . contents 
      package' = row ^. ix 2 . elements . contents 
  Upload date' author' package'

recentPackages :: Response ByteString -> [Upload]
recentPackages = toListOf 
               $ responseBody . to (decodeUtf8With lenientDecode) 
               . html . elements . elements . elements . elements 
               . named (only "table") . elements . children 
               . to table

main :: IO ()
main = get "https://hackage.haskell.org/packages/recent" >>= print `fmap` recentPackages 
