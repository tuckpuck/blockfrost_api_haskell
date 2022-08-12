{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FetchNetworkSupply where

import Env ( apiKey )
import Lib

import Network.Wreq
import Control.Lens
import Data.Aeson
import Text.Printf (printf)
import GHC.Generics


data SupplyAll = SupplyAll {
    supply :: Supply
} deriving (Show, Generic)

instance FromJSON SupplyAll

data Supply = Supply {
    circulating :: [Char],                 --The amount of coins that are circulating in the market and are in public hands. It is analogous to the flowing shares in the stock market.
    locked :: [Char],
    max :: [Char],                         --The maximum amount of coins that will ever exist in the lifetime of the cryptocurrency. It is analogous to the fully diluted shares in the stock market.
    reserves :: [Char],
    total :: [Char],                      --The amount of coins that have been already created, minus any coins that have been burned. It is analogous to the outstanding shares in the stock market.
    treasury :: [Char]
} deriving (Show, Generic)

instance FromJSON Supply

fetchNetworkSupply :: IO ()
fetchNetworkSupply = do
    let opts = defaults & header "project_id" .~ apiKey
    rsp <- asJSON =<< getWith opts "https://cardano-mainnet.blockfrost.io/api/v0/network" 
    let adaSupply :: SupplyAll
        adaSupply = rsp ^. responseBody 
    let adaSupplyDetails = supply adaSupply
    putStrLn ("     ")
    putStrLn ("*** SUPPLY ***                    ")
    putStrLn ("Max Supply                        " ++ formatLovelace (FetchNetworkSupply.max adaSupplyDetails) ++ " ₳")
    putStrLn ("Total Supply Available            " ++ formatLovelace (total adaSupplyDetails) ++ " ₳")
    putStrLn ("% Supply Available                " ++ printf "%.2g"  (((read (total adaSupplyDetails) :: Float) / (read (FetchNetworkSupply.max adaSupplyDetails) :: Float) * 100 )) ++ "%")
    putStrLn ("Remaining Reserve                 " ++ formatLovelace (reserves adaSupplyDetails) ++ " ₳")
    putStrLn ("% Supply in Reserve               " ++ printf "%.2g" (((read (reserves adaSupplyDetails) :: Double) / (read (FetchNetworkSupply.max adaSupplyDetails) :: Double) * 100)) ++ "%")
    putStrLn ("Cardano Treasury                  " ++ formatLovelace (treasury adaSupplyDetails) ++ " ₳")
    -- Others
    -- putStrLn ("Circulating Supply                " ++ formatLovelace (circulating adaSupplyDetails) ++ " ₳")
    -- putStrLn ("Locked                            " ++ formatLovelace (locked adaSupplyDetails) ++ " ₳")


