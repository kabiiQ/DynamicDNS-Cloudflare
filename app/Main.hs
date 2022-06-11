{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Configuration
import DNSAPI
import Control.Monad.Except
import Control.Exception
import Control.Concurrent (threadDelay)
import System.Exit (exitFailure)

main :: IO ()
main = catch application err

err :: SomeException -> IO ()
err e = do
    putStrLn $ show e ++ "\n\nThe application encountered an error!\nPress return to exit."
    _ <- getLine
    return ()

application :: IO ()
application = do
    -- load user configuration
    config <- loadConfig "cloudflare.dns"

    -- call to validate cloudflare identity, dns record existence, and current external address
    zoneId <- cfGetZone config
    record <- cfGetRecord config zoneId
    putStrLn $ "Cloudflare has IP " ++ recordContent record ++ " saved for " ++ recordId record ++ "."
    putStrLn $ "Checking external IP every " ++ show (frequencySeconds config) ++ " seconds."

    -- start main application loop - takes current 'stored' PI, and values needed to update record if changed
    applicationLoop record config zoneId

-- main application loop
-- poll current external IP, and update with cloudflare if changed
applicationLoop :: CFRecord -> CloudflareConfig -> String -> IO ()
applicationLoop storedRecord cfg zoneId = do
    -- get current external IP
    ip <- runExceptT $ queryAddr cfg
    case ip of
        Left err -> do
            putStrLn $ "Failed to get IP " ++ show err
            threadDelay $ timeoutInterval cfg
            applicationLoop storedRecord cfg zoneId -- loop with no changes after timeout
        Right addr -> do
            -- compare stored IP to retreived IP
            if addr == recordContent storedRecord then do
                -- IP has not changed
                putStrLn $ "External IP: " ++ addr ++ ". No change needed."
                threadDelay $ repeatInterval cfg
                applicationLoop storedRecord cfg zoneId -- loop with no changes after regular delay interval
            else do
                -- IP address changed
                change <- runExceptT $ cfUpdateRecord cfg storedRecord zoneId addr
                case change of 
                    Left err -> do
                        putStrLn $ "Unable to update your Cloudflare record! :: " ++ show err
                        exitFailure
                    Right _ -> do
                        putStrLn $ "Cloudflare IP has been updated to " ++ addr
                        threadDelay $ repeatInterval cfg
                        let newRecord = storedRecord { recordContent = addr }
                        applicationLoop newRecord cfg zoneId -- loop with new IP change recorded