{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Web.Actions.User where

import           Model.CoreTypes
import           Model.ResponseTypes
import           Web.Utils

import           Control.Monad
import           Control.Monad.Trans
import qualified Crypto.Hash.SHA512   as SHA
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as Text
import           Data.Time
import           Data.Word8
import           Database.Persist
import           Database.Persist.Sql
import           System.Random


randomBytes:: Int -> StdGen -> [Word8]
randomBytes 0 _ = []
randomBytes ct g =
    let (value, nextG) = next g
    in fromIntegral value:randomBytes (ct - 1) nextG

randomBS :: Int -> StdGen -> ByteString
randomBS len g =
    BS.pack $ randomBytes len g

hashPassword :: Text -> ByteString -> ByteString
hashPassword password salt =
    SHA.finalize $ SHA.updates SHA.init [salt, Text.encodeUtf8 password]

createSession :: UserId -> SqlPersistM SessionId
createSession userId = do
    now <- liftIO getCurrentTime
    insert $ Session (addUTCTime (5 * 3600) now) userId

killSessions :: UserId -> SqlPersistM ()
killSessions userId =
    deleteWhere [ SessionUserId ==. userId ]

loginUser :: Text -> Text -> SqlPersistM (Maybe UserId)
loginUser username password = do
    mUserU <- getBy (UniqueUsername username)
    mUserE <- getBy (UniqueEmail username)
    pure $ case mUserU `mplus` mUserE of
        Just userEntity -> let
            user = entityVal userEntity
            inputPasswordHash =
                makeHex $ hashPassword password $ decodeHex $ userSalt user
            in
            if userPassword user == inputPasswordHash then
                Just $ entityKey userEntity
            else
                Nothing
        Nothing -> Nothing

loadUser :: SessionId -> SqlPersistM (Maybe (UserId, User))
loadUser sessId = do
    mSess <- get sessId
    now <- liftIO getCurrentTime
    case mSess of
        Just sess | sessionValidUntil sess > now -> do
            mUser <- get $ sessionUserId sess
            pure $ fmap (sessionUserId sess,) mUser
        _ ->
            pure Nothing


registerUser :: Text -> Text -> Text -> SqlPersistM CommonResponse
registerUser username email password = do
    mUserU <- getBy $ UniqueUsername username
    mUserE <- getBy $ UniqueEmail email
    case (mUserU, mUserE) of
        (Just _, _) -> pure $ CommonError "Username already taken!"
        (_, Just _) -> pure $ CommonError "Email already registered!"
        (Nothing, Nothing) -> do
            g <- liftIO getStdGen
            let salt = randomBS 512 g
                hash = hashPassword password salt
            _ <-
                insert $
                User username (makeHex hash) (makeHex salt) email False False
            pure $ CommonSuccess "Signup complete. You may now login."
