module Main where

import qualified FetchBlockByBlockHeight
import qualified FetchAssetByPolicyID
import qualified FetchNetworkParameters
import qualified FetchCurrentEpochData
import qualified FetchNetworkSupply
import qualified FetchNetworkStake


main :: IO ()
main = do
    putStrLn "What would you like to see?"
    userInput <- getLine 
    if (userInput == "Parameters" || userInput == "parameters" || userInput == "Params" || userInput == "params") then 
        FetchNetworkParameters.fetchNetworkParameters
    else if (userInput == "Supply" || userInput == "supply") then
        FetchNetworkSupply.fetchNetworkSupply
    else if (userInput == "Stake" || userInput == "stake" || userInput == "Staking" || userInput == "staking") then
        FetchNetworkStake.fetchNetworkStake
    else if (userInput == "Epoch" || userInput == "epoch") then
        FetchCurrentEpochData.fetchCurrentEpochData
     else if (userInput == "Network" || userInput == "network") then
        fetchNetwork
    else if (userInput == "Block" || userInput == "block") then do
        putStrLn "Which block would you like information about?"
        putStrLn "Please provide a block height Number:"
        blockId <- getLine  
        FetchBlockByBlockHeight.fetchBlockByBlockHeight blockId
    else if (userInput == "Asset" || userInput == "asset" || userInput == "nft" || userInput == "NFT") then do
        putStrLn "Which asset would you like information about?"
        putStrLn "Please provide a policy ID number:"
        assetID <- getLine  
        FetchAssetByPolicyID.fetchAssetByPolicyID assetID
    else if (userInput == "All" || userInput == "all" || userInput == "Test" || userInput == "test") then
        fetchAll 
    else
        putStrLn "Enter a known keyword"


fetchNetwork :: IO()
fetchNetwork = do 
    FetchNetworkParameters.fetchNetworkParameters
    FetchNetworkSupply.fetchNetworkSupply
    FetchNetworkStake.fetchNetworkStake
    FetchCurrentEpochData.fetchCurrentEpochData

fetchAll :: IO()
fetchAll = do 
    FetchNetworkParameters.fetchNetworkParameters
    FetchNetworkSupply.fetchNetworkSupply
    FetchNetworkStake.fetchNetworkStake
    FetchCurrentEpochData.fetchCurrentEpochData
    FetchBlockByBlockHeight.fetchBlockByBlockHeight "7584268"
    FetchAssetByPolicyID.fetchAssetByPolicyID "6dad8c414e24f941eedb8275c0a1d341e697d584b1b92f4a5193a62f.aeoniumskyB1001"
