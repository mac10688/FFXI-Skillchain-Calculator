{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Monoid (mconcat)
import Skillchain

main = scotty 3000 $
    get "/sc/:w1/:w2" $ do
        w1 <- read <$> param "w1"
        w2 <- read <$> param "w2"
        json $ map (\(ws1, ws2, sc) -> 
          (weaponskillName ws1, weaponskillName ws2, sc)) 
          $ findAllScForWeapons w1 w2
