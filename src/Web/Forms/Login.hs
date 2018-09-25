{-# LANGUAGE OverloadedStrings #-}
module Web.Forms.Login where

import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Text.Blaze.Html  (Html)

import           Web.Forms.Common

data LoginRequest = LoginRequest
   { lr_user     :: Text
   , lr_password :: Text
   }
   deriving (Show)

loginForm :: Monad m => Form Html m LoginRequest
loginForm = LoginRequest
    <$> "name"     .: usernameFormlet Nothing
    <*> "password" .: passwordFormlet Nothing

loginFormSpec :: FormMeta
loginFormSpec = FormMeta
    { fm_method = POST
    , fm_target = "/login"
    , fm_components =
        [ FCSection FormSection
            { fs_title = Nothing
            , fs_help = Nothing
            , fs_elements =
                [ FormElement
                    "name" (Just "Username") (Just "Username") InputText
                , FormElement
                    "password" (Just "Password") (Just "Password") InputPassword
                ]
            }
        ]
    , fm_submitValue = "Login"
    }
