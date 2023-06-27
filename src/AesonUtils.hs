module AesonUtils (dropPrefixAesonOptions) where

import Data.Aeson.TH (Options, defaultOptions, fieldLabelModifier)
import Data.Char (toLower)

lower1 :: String -> String
lower1 (c : cs) = toLower c : cs
lower1 [] = []

dropPrefixAesonOptions :: String -> Options
dropPrefixAesonOptions prefix =
  defaultOptions
    { fieldLabelModifier = lower1 . drop (length prefix)
    }
