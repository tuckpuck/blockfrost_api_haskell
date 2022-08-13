{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FetchNetworkParameters where

import Env ( apiKey )
import Lib

import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Text
import GHC.Generics


data NetworkParameters = NetworkParameters {
    epoch :: Int,                           --The first epoch for which these parameters are valid.
    min_fee_a :: Int,                       --The 'a' parameter to calculate the minimum transaction fee.
    min_fee_b :: Int,                       --The 'b' parameter to calculate the minimum transaction fee.
    max_block_size :: Int,                  --The maximum block size (in bytes).
    max_tx_size :: Int,                     --The maximum transaction size (in bytes).
    max_block_header_size :: Int,           --The maximum block header size (in bytes).
    key_deposit :: Text,                    --The amount (in Lovelace) require for a deposit to register a StakeAddress.
    pool_deposit :: Text,                   --The amount (in Lovelace) require for a deposit to register a stake pool.
    e_max :: Int,                           --The maximum number of epochs in the future that a pool retirement is allowed to be scheduled for.
    n_opt :: Int,                           --The optimal number of stake pools.
    a0 :: Float,                            --The influence of the pledge on a stake pool's probability on minting a block.
    rho :: Double,                          --The monetary expansion rate. % of the Reserve identified as being the potential total possible rewards and the treasury cut for that epoch
    tau :: Float,                           --The treasury growth rate.
    decentralisation_param :: Int,          --The decentralisation parameter (1 fully centralised, 0 fully decentralised).
    extra_entropy :: Maybe Float,           --The 32 byte string of extra random-ness to be added into the protocol's entropy pool.
    protocol_major_ver :: Int,              --The protocol major number.
    protocol_minor_ver :: Int,              --The protocol minor number.
    min_utxo :: Text,                       --The minimum value of a UTxO entry.
    min_pool_cost :: Text,                  --The minimum pool cost.
    nonce :: Text,                          --The nonce value for this epoch.       
    price_mem :: Double,                    --The per word cost of script memory usage.
    price_step :: Double,                   --The cost of script execution step usage.
    max_tx_ex_mem :: Text,                  --The maximum number of execution memory allowed to be used in a single transaction.
    max_tx_ex_steps :: Text,                --The maximum number of execution steps allowed to be used in a single transaction.
    max_block_ex_mem :: Text,               --The maximum number of execution memory allowed to be used in a single block.
    max_block_ex_steps :: Text,             --The maximum number of execution steps allowed to be used in a single block.
    max_val_size :: Text,                   --The maximum Val size.
    collateral_percent :: Int,              --The percentage of the txfee which must be provided as collateral when including non-native scripts.
    max_collateral_inputs :: Int,           --The maximum number of collateral inputs allowed in a transaction.
    coins_per_utxo_word :: Text             --The cost per UTxO word.
} deriving (Show, Generic)

instance FromJSON NetworkParameters

fetchNetworkParameters :: IO ()
fetchNetworkParameters = do
    let opts = defaults & header "project_id" .~ apiKey
    rsp <- asJSON =<< getWith opts "https://cardano-mainnet.blockfrost.io/api/v0/epochs/latest/parameters" 
    let parameters :: NetworkParameters
        parameters = rsp ^. responseBody
    putStrLn ("     ")
    putStrLn ("*** NETWORK PARAMETERS ***                  ")
    putStrLn ("Decentralization Parameter        " ++ show (decentralisation_param parameters))
    putStrLn ("Optimal Number of Stakepools (k)  " ++ show (n_opt parameters))
    putStrLn ("Pledge Influence Parameter (a0)   " ++ show (a0 parameters))
    putStrLn ("Monetary Expansion Rate           " ++ showFullPrecision ((rho parameters) * 100) ++ "%")
    putStrLn ("Treasury Growth Rate              " ++ show ((tau parameters) * 100) ++ "%")
    putStrLn ("Version                           " ++ show (protocol_major_ver parameters) ++ "." ++ show (protocol_minor_ver parameters))
    putStrLn ("Max Block Size                    " ++ show (max_block_size parameters) ++ " bytes")
    putStrLn ("Max Transaction Size              " ++ show (max_tx_size parameters) ++ " bytes")
    putStrLn ("Max Block Header Size             " ++ show (max_block_header_size parameters) ++ " bytes")
    -- Others
    -- putStrLn ("Epoch                             " ++ show (epoch parameters))
    -- putStrLn ("Min Fee A                         " ++ show (min_fee_a parameters))
    -- putStrLn ("Min Fee B                         " ++ show (min_fee_b parameters))
    -- putStrLn ("Key Deposit                       " ++ show (key_deposit parameters))
    -- putStrLn ("Pool Deposit                      " ++ show (pool_deposit parameters))
    -- putStrLn ("Max Epochs to Retire pool         " ++ show (e_max parameters))
    -- putStrLn ("Extra Entropy                     " ++ show (extra_entropy parameters))
    -- putStrLn ("Minimum value of EUTXO entry      " ++ show (min_utxo parameters))
    -- putStrLn ("Minimum Pool Cost                 " ++ show (min_pool_cost parameters))
    -- putStrLn ("Nonce                             " ++ show (nonce parameters))
    -- putStrLn ("Per-Word Cost of Script Memory    " ++ show (price_mem parameters))
    -- putStrLn ("Script Memory Usage Cost          " ++ show (price_step parameters))
    -- putStrLn ("Max Execution Memory Allowed per Tx" ++ show (max_tx_ex_mem parameters))
    -- putStrLn ("Max Execution Steps Allowed per Tx" ++ show (max_tx_ex_steps parameters))
    -- putStrLn ("Max Execution Memory Allowed per Block" ++ show (max_block_ex_mem parameters))
    -- putStrLn ("Max Execution Steps Allowed per Tx" ++ show (max_block_ex_steps parameters))
    -- putStrLn ("Maximum Val size                  " ++ show (max_val_size parameters))
    -- putStrLn ("Percent of Tx Fee Used as Collateral" ++ show (collateral_percent parameters))
    -- putStrLn ("Max Collateral Inputs Allowed in a Tx" ++ show (max_collateral_inputs parameters))
    -- putStrLn ("Cost Per UTXO Word                " ++ show (coins_per_utxo_word parameters))






