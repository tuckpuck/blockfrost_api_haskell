{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module FetchAssetByPolicyID where

import Env ( apiKey )
import Lib

import Network.Wreq
import Control.Lens
import Data.Aeson
import GHC.Generics
import Data.Aeson.TH


data File = File {
    mediaType :: String,
    src :: String
} deriving (Show, Generic)

instance FromJSON File

data Metadata = Metadata {
    fourK_mp4 :: String,
    eightK_jpg :: String,
    copyright :: String,
    files :: [File],
    image :: String
} deriving (Show, Generic)

$(deriveFromJSON defaultOptions {
    fieldLabelModifier = let f "fourK_mp4" = "4K mp4"
                             f "eightK_jpg" = "8K jpg"
                             f "copyright" = "Copyright"
                             f "files" = "files"
                             f "image" = "image"
                             f _ = "N/A"
                         in f
} ''Metadata)

data Trait = Trait {
    collection :: String,
    instagram :: String,
    number :: String,
    rarity :: String,
    twitter :: String,
    website :: String
} deriving (Show, Generic)

$(deriveFromJSON defaultOptions {
    fieldLabelModifier = let f "collection" = "Collection"
                             f "instagram" = "Instagram"
                             f "number" = "Number"
                             f "rarity" = "Rarity"
                             f "twitter" = "Twitter"
                             f "website" = "Website"
                             f _ = "N/A"
                         in f
} ''Trait)

data Project = Project {
    slug :: String,
    new_slug :: String,
    thumbnail :: String
} deriving (Show, Generic)

instance FromJSON Project

data Asset = Asset {
    fingerprint :: [Char], 
    policy :: [Char], 
    name :: [Char],
    last_metadata :: Metadata,
    traits :: Trait,
    created_at :: [Char],
    updated_at :: [Char],
    statistical_rank :: Int,
    statistical_score :: Double,
    rarity_rank :: Int,
    rarity_score :: Double,
    project :: Project,
    attribution :: [Char]
} deriving (Show, Generic)

instance FromJSON Asset 

fetchAssetByPolicyID :: [Char] -> IO ()
fetchAssetByPolicyID x = do
    let opts = defaults & header "project_id" .~ apiKey
    rsp <- asJSON =<< getWith opts ("https://api.opencnft.io/1/asset/" ++ x)
    let assetInfo :: Asset
        assetInfo = rsp ^. responseBody
    let traitInfo = traits assetInfo
    let projectInfo = project assetInfo
    let metadataInfo = last_metadata assetInfo
    let filesInfo = files metadataInfo

    putStrLn ("     ")
    putStrLn ("*** ASSET INFORMATION ***         ")
    putStrLn ("Asset Name                        " ++ (name assetInfo))
    putStrLn ("Number                            " ++ (number traitInfo))
    putStrLn ("Collection                        " ++ (collection traitInfo))
    putStrLn ("4k MP4                            " ++ (fourK_mp4 metadataInfo))
    putStrLn ("8k JPG                            " ++ (eightK_jpg metadataInfo))
    putStrLn ("Image                             " ++ (image metadataInfo))
    putStrLn ("Created at                        " ++ (created_at assetInfo))
    putStrLn ("Updated at                        " ++ (updated_at assetInfo))
    putStrLn ("Statistical Rank                  " ++ show (statistical_rank assetInfo))
    putStrLn ("Statistical Score                 " ++ showFullPrecision (statistical_score assetInfo))
    putStrLn ("Rarity Rank                       " ++ show (rarity_rank assetInfo))
    putStrLn ("Rarity Score                      " ++ showFullPrecision (rarity_score assetInfo))
    putStrLn ("Rarity                            " ++ (rarity traitInfo))
    putStrLn ("Instagram                         " ++ (instagram traitInfo))
    putStrLn ("Twitter                           " ++ (twitter traitInfo))
    putStrLn ("Website                           " ++ (website traitInfo))
    putStrLn ("Policy ID                         " ++ (policy assetInfo))
    putStrLn ("Thumbnail                         " ++ (thumbnail projectInfo))
    putStrLn ("Fingerprint                       " ++ (fingerprint assetInfo))
    putStrLn ("Copyright                         " ++ (copyright metadataInfo))
    putStrLn ("NFT Data provided by OpenCNFT.io")
    -- Others
    -- putStrLn ("New slug                          " ++ (new_slug projectInfo))
    -- putStrLn ("Slug                              " ++ (slug projectInfo))
    -- putStrLn ("Media Type                        " ++ (mediaType (filesInfo !! 0)))
    -- putStrLn ("Attribution                       " ++ (attribution assetInfo))
