{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Network.Wai.Middleware.Cors

import Data.Monoid (mconcat)
import Skillchain

import GHC.Generics as Generics
import Data.Aeson as Aeson

main :: IO ()
main = scotty 3000 $ do
  middleware simpleCors
  get "/sc/:w1/:w2" $ do
    w1 <- read <$> param "w1"
    w2 <- read <$> param "w2"
    Web.Scotty.json $ map (\(ws1, ws2, sc) -> 
      Ws (weaponskillName ws1) (weaponskillName ws2) sc) 
      $ findAllScForWeapons w1 w2


data WsComboInfo = Ws
  {
    ws1 :: String
  , ws2 :: String
  , sc :: SkillchainAttr
  }
  deriving (Generic)

instance ToJSON WsComboInfo
