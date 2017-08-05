{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Json where

import qualified Network.Wreq as NW
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Data.Text
import Data.Maybe
import GHC.Generics
import Data.Map (fromListWith, toList)

main	= do
	r <- NW.get "https://jsonplaceholder.typicode.com/posts"
	let jsonData = decode (r ^. NW.responseBody) :: Maybe [MyData]
	let count = frequency $ getCount jsonData
	print (count)
	
	
data MyData = MyData { userId :: Int, id :: Int, title :: String, body :: String }
  deriving (Generic, Show)

instance FromJSON MyData where
 parseJSON (Object v) =
    MyData <$> v .: "userId"
           <*> v .: "id"
           <*> v .: "title"
           <*> v .: "body"
 parseJSON _ = mzero
	
	
getCount	:: Maybe [MyData] -> [Int]
getCount	= f where
	f (Just mds)	= Prelude.map g mds
	f Nothing		= error "Empty json file"
	g (MyData {userId = us})	= us
	
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

{- REFERENCES:
https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
https://stackoverflow.com/questions/29941866/parsing-json-data-from-a-url-in-haskell-using-aeson
http://deathbytape.com/articles/2015/02/09/json-parse-haskell.html
-}