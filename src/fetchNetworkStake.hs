{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FetchNetworkStake where

import Env ( apiKey )
import Lib

import Network.Wreq
import Control.Lens
import Data.Aeson
import GHC.Generics


data StakeAll = StakeAll {
    stake :: Stake
} deriving (Show, Generic)

instance FromJSON StakeAll 

data Stake = Stake {
    active :: [Char],
    live :: [Char]
} deriving (Show, Generic)

instance FromJSON Stake 

fetchNetworkStake :: IO ()
fetchNetworkStake = do
    let opts = defaults & header "project_id" .~ apiKey
    rsp <- asJSON =<< getWith opts "https://cardano-mainnet.blockfrost.io/api/v0/network" 
    let adaStaked :: StakeAll
        adaStaked = rsp ^. responseBody
    let stakedDetails = stake adaStaked
    putStrLn ("     ")
    putStrLn ("*** STAKE ***                  ")
    putStrLn ("Active Stake                      " ++ formatLovelace (active stakedDetails)  ++ " ₳")
    putStrLn ("Live Stake                        " ++ formatLovelace (live stakedDetails) ++ " ₳")
    putStrLn ("Live but not Active yet           " ++ formatCommas (addCommas (lovelaceToAda (readAsInteger (live stakedDetails) - readAsInteger (active stakedDetails))))  ++ " ₳")

