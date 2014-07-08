{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens (to, only,(^?),ix, toListOf)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Network.HTTP.Client (Response)
import Network.Wreq (responseBody, get)
import Text.Taggy (Node)
import Text.Taggy.Lens (html, elements, children, contents,allNamed)

data Upload = 
  Upload Text -- ^ date
         Text -- ^ author
         Text -- ^ package name
  deriving (Show)

table :: [Node] -> Maybe Upload
table row = do
  date    <- row ^? ix 0 . contents 
  author  <- row ^? ix 1 . contents 
  package <- row ^? ix 2 . elements . contents 
  return $ Upload date author package

recentPackages :: Response ByteString -> [Maybe Upload]
recentPackages = toListOf 
               $ responseBody . to (decodeUtf8With lenientDecode) 
               . html . allNamed (only "tr") . children . to table

main :: IO ()
main = get "https://hackage.haskell.org/packages/recent" >>= print `fmap` recentPackages 
