{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FetchCurrentEpochData where

import Env ( apiKey )
import Lib

import Network.Wreq
import Control.Lens
import Data.Aeson
import GHC.Generics


data Epoch = Epoch {
    epoch :: Int,
    start_time :: Int,
    end_time :: Int,
    first_block_time :: Int, 
    last_block_time :: Int,
    block_count :: Int,
    tx_count :: Int,
    output :: [Char],
    fees :: [Char],
    active_stake :: [Char]
} deriving (Show, Generic)

instance FromJSON Epoch 

fetchCurrentEpochData :: IO ()
fetchCurrentEpochData = do
    let opts = defaults & header "project_id" .~ apiKey
    rsp <- asJSON =<< getWith opts "https://cardano-mainnet.blockfrost.io/api/v0/epochs/latest" 
    let currentEpoch :: Epoch
        currentEpoch = rsp ^. responseBody
    putStrLn ("     ")
    putStrLn ("*** CURRENT EPOCH [" ++ show (epoch currentEpoch) ++ "] ***             ")
    putStrLn ("Epoch                             " ++ show (epoch currentEpoch))
    putStrLn ("Block Count                       " ++ formatInt (block_count currentEpoch))
    putStrLn ("Transaction Count                 " ++ formatInt (tx_count currentEpoch))
    putStrLn ("Fees                              " ++ formatLovelace (fees currentEpoch) ++ " ₳")
    putStrLn ("Output                            " ++ formatLovelace (output currentEpoch) ++ " ₳")
    putStrLn ("Start Time                        " ++ posixToUTC (start_time currentEpoch))
    putStrLn ("End Time                          " ++ posixToUTC (end_time currentEpoch))
    -- Others
    -- putStrLn ("Active Stake                      " ++ formatLovelace (active_stake currentEpoch) ++ " ₳")
    -- putStrLn ("First Block Time                  " ++ posixToUTC (first_block_time currentEpoch))
    -- putStrLn ("Last Block Time                   " ++ posixToUTC (last_block_time currentEpoch))