{-# LANGUAGE CPP           #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import GHC.Generics
import Data.List
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Trans.Either

data Artist = Artist
  { artistId :: Int
  , name :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Artist

type ArtistAPI =
      Get '[JSON] [Artist]
  :<|> Capture "artistId" Int :> Get '[JSON] Artist

artistsServer :: Server ArtistAPI
artistsServer = getArtists :<|> artistOperations

  where getArtists :: EitherT ServantErr IO [Artist]
        getArtists = return artists

        artistOperations artistId =
          viewArtist

          where viewArtist :: EitherT ServantErr IO Artist
                viewArtist = artistById artistId

artistById :: Int -> EitherT ServantErr IO Artist
artistById idParam =
  case a of
    Nothing -> left (err404 {errBody = "No artist with given id exists"})
    Just b -> return b
  where
    a = find ((== idParam) . artistId) artists

artists :: [Artist]
artists =
  [ Artist 1 "Aventura"
  , Artist 2 "Prince Royce"
  , Artist 3 "Xtreme"
  , Artist 4 "Antony Santos"
  , Artist 5 "Monchy Y Alexandra"
  , Artist 6 "Carlos y Alejandra"
  , Artist 7 "Bachata Heightz"
  ]

type API = "artists" :> ArtistAPI

api :: Proxy API
api = Proxy

app :: Application
app = serve api artistsServer

main :: IO ()
main = run 8081 app
