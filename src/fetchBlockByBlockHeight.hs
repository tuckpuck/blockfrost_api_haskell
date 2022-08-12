{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FetchBlockByBlockHeight where

import Env ( apiKey )
import Lib

import Network.Wreq
import Control.Lens
import Data.Aeson
import GHC.Generics


data Block = Block {
    time :: Int,
    height :: Int,
    hash :: [Char],
    slot :: Int,
    epoch :: Int,
    epoch_slot :: Int,
    slot_leader :: [Char],
    size :: Int,
    tx_count :: Int,
    output :: Maybe [Char],
    fees :: Maybe [Char],
    block_vrf :: Maybe [Char],
    previous_block :: [Char],
    next_block :: [Char],
    confirmations :: Int
} deriving (Show, Generic)

instance FromJSON Block 

fetchBlockByBlockHeight :: [Char] -> IO ()
fetchBlockByBlockHeight x = do
    let opts = defaults & header "project_id" .~ apiKey
    rsp <- asJSON =<< getWith opts ("https://cardano-mainnet.blockfrost.io/api/v0/blocks/" ++ x)
    let blockInfo :: Block
        blockInfo = rsp ^. responseBody

    putStrLn ("     ")
    putStrLn ("*** BLOCK " ++ show (height blockInfo) ++ " ***                  ")
    putStrLn ("Epoch                             " ++ show (FetchBlockByBlockHeight.epoch blockInfo))
    putStrLn ("Block Height                      " ++ formatCommas (addCommas (height blockInfo)))
    putStrLn ("Epoch Slot                        " ++ formatCommas (addCommas  (epoch_slot blockInfo)))
    putStrLn ("Slot                              " ++ formatCommas (addCommas (slot blockInfo)))
    putStrLn ("Confirmations                     " ++ formatCommas (addCommas (confirmations blockInfo)))
    putStrLn ("Mint Time                         " ++ posixToUTC (time blockInfo))
    putStrLn ("Slot Leader                       " ++ (slot_leader blockInfo))
    putStrLn ("Size                              " ++ formatCommas (addCommas (size blockInfo)) ++ " bytes")
    putStrLn ("Transaction Count                 " ++ show (FetchBlockByBlockHeight.tx_count blockInfo))
    putStrLn ("Output                            " ++ formatMaybeToLovelace (FetchBlockByBlockHeight.output blockInfo) ++ " ₳")
    putStrLn ("Fees                              " ++ formatMaybeToLovelace (FetchBlockByBlockHeight.fees blockInfo) ++ " ₳")
    putStrLn ("Block VRF                         " ++ printMaybe (block_vrf blockInfo))
    putStrLn ("Block Hash                        " ++ (hash blockInfo))
    putStrLn ("Previous Block                    " ++ (previous_block blockInfo))
    putStrLn ("Next Block                        " ++ (next_block blockInfo))


