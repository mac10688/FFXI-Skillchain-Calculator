{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Data.Monoid (mconcat)
import Skillchain

import GHC.Generics as Generics
import Data.Aeson as Aeson

import Control.Monad.IO.Class

main :: IO ()
main = scotty 3000 $ do
  middleware simpleCors
  middleware logStdoutDev
  --get "/sc/:w1/:w2" $ do
    --w1 <- read <$> param "w1"
    --w2 <- read <$> param "w2"
    --Web.Scotty.json $ map (\(ws1, ws2, sc) -> 
      --Ws (weaponskillName ws1) (weaponskillName ws2) sc) 
      -- $ findAllScForWeapons w1 w2
  get "/sc/:weapons" $ do
    array <- (param "weapons" :: ActionM [String])
    liftIO $ putStrLn (show array)
    return ()


data WsComboInfo = Ws
  {
    ws1 :: String
  , ws2 :: String
  , sc :: SkillchainAttr
  }
  deriving (Generic)

instance ToJSON WsComboInfo
