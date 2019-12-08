{-# LANGUAGE OverloadedStrings #-}

import Skillchain

import Control.Monad.IO.Class

main :: IO ()
main = putStrLn $ show $ scCombinations [Axe, Axe, Axe, Axe, Axe, Axe]
  
