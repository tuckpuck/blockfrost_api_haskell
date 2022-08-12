{-# LANGUAGE OverloadedStrings #-}

module Env where 

import Data.Text
import Data.Text.Encoding

-- To get this project working, add your Blockfrost API key into 'apiKeyOriginal' variable below. Then remove the . in the filename before .Env. 

apiKeyOriginal :: Text 
apiKeyOriginal = ""

apiKey = [(Data.Text.Encoding.encodeUtf8 apiKeyOriginal)]