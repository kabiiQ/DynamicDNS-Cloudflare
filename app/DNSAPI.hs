{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module DNSAPI where

import Configuration
import Data.Maybe
import Network.Wreq
import Control.Lens((&), (.~))
import Data.ByteString.Lazy.Char8 as C
import Data.ByteString.UTF8 as BL
import Control.Exception
import Data.Aeson(toJSON, ToJSON, object, (.=))
import Data.Aeson.Lens (_String, _Array, key)
import Control.Lens.Getter
import Control.Lens.Fold
import Data.Text(unpack)
import Control.Monad.Except
import qualified Language.Haskell.TH.Lens as A

type CloudflareMonad = ExceptT String IO -- either error string or success IO value

queryAddr :: CloudflareConfig -> CloudflareMonad String
queryAddr c = do
    let rs = try $ get $ ipService c :: IO (Either SomeException (Response C.ByteString))
    r <- liftIO rs
    either (throwError . show) (\x -> return $ C.unpack (x ^. responseBody)) r

withHeaders :: CloudflareConfig -> Options
withHeaders cfg = defaults & email & key & agent
    where email = header "X-Auth-Email" .~ [BL.fromString $ cfEmail cfg]
          key = header "X-Auth-Key" .~ [BL.fromString $ cfKey cfg]
          agent = header "User-Agent" .~ [BL.fromString "kabii/1.0 DynamicDNS"]

cfGetZone :: CloudflareConfig -> IO String
cfGetZone cfg = do
    r <- getWith (withHeaders cfg) ("https://api.cloudflare.com/client/v4/zones?name=" ++ cfDomainName cfg)
    json <- asValue r
    let [zone] = toListOf(responseBody . key "result" . _Array . traverse) json
    return $ Data.Text.unpack $ view (key "id" . _String) zone

data CFRecord = CFRecord {
    recordId :: String
    , recordContent :: String
    , recordStaticName :: String
}

cfGetRecord :: CloudflareConfig -> String -> IO CFRecord
cfGetRecord cfg zoneId = do
    let name = cfRecordName cfg
    r <- getWith (withHeaders cfg) ("https://api.cloudflare.com/client/v4/zones/" ++ zoneId ++ "/dns_records?type=A&name=" ++ name)
    json <- asValue r
    let [record] = toListOf(responseBody . key "result" . _Array . traverse) json
    let recordId = Data.Text.unpack $ view (key "id" . _String) record
    let content = Data.Text.unpack $ view (key "content" . _String) record
    return $ CFRecord recordId content name

data CFRecordUpdate = CFRecordUpdate {
    recordName :: String
    , newContent :: String
}

instance ToJSON CFRecordUpdate where
    -- Record type will always be A.
    -- Must create entire record update as it will be overwritten in POST
    -- record name should be sent along, but not changed 
    toJSON (CFRecordUpdate x y) = object ["type" .= ("A" :: String), "name" .= x, "content" .= y]

cfUpdateRecord :: CloudflareConfig -> CFRecord -> String -> String -> CloudflareMonad ()
cfUpdateRecord cfg record zoneId newIP = do
    let update = CFRecordUpdate (recordStaticName record) newIP
    let rs = try $ putWith (withHeaders cfg) ("https://api.cloudflare.com/client/v4/zones/" ++ zoneId ++ "/dns_records/" ++ recordId record) (toJSON update) :: IO (Either SomeException (Response C.ByteString))
    r <- liftIO rs 
    either (throwError . show) (\x -> return ()) r
