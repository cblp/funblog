{-# LANGUAGE OverloadedStrings #-}

module Web.Forms.Common where

import           Data.Maybe (isJust)
import           Data.Text  (Text)
import qualified Data.Text  as Text

minMaxLen :: (Int, Int) -> Text -> Either String Text
minMaxLen (minLen, maxLen) t
    | len >= minLen && len <= maxLen = Right stripped
    | otherwise = Left $ unwords
        [ "Must be longer than", show minLen, "and shorter than", show maxLen
        , "characters"
        ]
  where
    stripped = Text.strip t
    len = Text.length stripped

usernameFormlet :: Text -> Either String Text
usernameFormlet = minMaxLen (3, 12)

passwordFormlet :: Text -> Either String Text
passwordFormlet = minMaxLen (6, 40)

emailFormlet :: Text -> Either String Text
emailFormlet mTxt =
    check "Not a valid email address" (isJust . Text.find (== '@')) =<<
    minMaxLen (4, 50) mTxt

check :: e -> (a -> Bool) -> a -> Either e a
check e p a
    | p a       = Right a
    | otherwise = Left e
