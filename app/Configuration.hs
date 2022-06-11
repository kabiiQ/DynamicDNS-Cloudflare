module Configuration where

import Data.ConfigFile
import Control.Monad.Error

data CloudflareConfig = CloudflareConfig {
    cfEmail :: String
    , cfKey :: String
    , cfDomainName :: String
    , cfRecordName :: String
    , frequencySeconds :: Int
    , retryTimeoutSeconds :: Int
    , ipService :: String
}

repeatInterval :: CloudflareConfig -> Int
repeatInterval cfg = frequencySeconds cfg * 1000000

timeoutInterval :: CloudflareConfig -> Int
timeoutInterval cfg = retryTimeoutSeconds cfg * 1000000

loadConfig :: String -> IO CloudflareConfig
loadConfig fileName = do
    rv <- runErrorT $
        do
        config <- join $ liftIO $ readfile emptyCP fileName
        let c = config
        email <- get c "DEFAULT" "cloudflare_email"
        key <- get c "DEFAULT" "cloudflare_key"
        domain <- get c "DEFAULT" "cf_domain_name"
        record <- get c "DEFAULT" "cf_record_name"
        freqSec <- get c "DEFAULT" "frequency_seconds"
        retryTimeout <- get c "DEFAULT" "retry_timeout_seconds"
        ipService <- get c "DEFAULT" "ip_service"

        let freq = read freqSec :: Int
        let timeout = read retryTimeout :: Int

        return (CloudflareConfig email key domain record freq timeout ipService)

    either (error . snd) return rv