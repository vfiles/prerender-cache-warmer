{-# LANGUAGE OverloadedStrings          #-}

{-|
Module      : Main
Description : cache warmer for prerender
Copyright   : ©2014 VFILES, LLC
License     : AllRightsReserved
Maintainer  : engineering@vfiles.com
Stability   : experimental
Portability : POSIX

-}

module Main where

------------------------------------------------------------------------------
import Control.Concurrent.Async     (mapConcurrently)
import Control.Exception
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Class    (lift)
import Control.Retry
import Data.Aeson
import Data.List.Split              (chunksOf)
import Data.Monoid
import Data.String.Conv
import Data.Time                    (getCurrentTime
                                    ,diffUTCTime)
import Network.HTTP.Conduit         (parseUrl
                                    ,http
                                    ,Request(..)
                                    ,RequestBody(..)
                                    ,Response(..)
                                    ,Manager
                                    ,setQueryString
                                    ,tlsManagerSettings
                                    ,newManager)
import Network.HTTP.Types.Status    (Status(..))
import System.Environment           (getArgs)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

crawl :: Manager -> String -> IO ()
crawl manager url = do
  start <- getCurrentTime
  initReq <- parseUrl url
  let hdr = [("User-Agent", "Googlebot/2.1 (+http://www.google.com/bot.html)")]
      req = initReq { requestHeaders = hdr }
  runResourceT $ do
    resp <- http req manager
    stop <- lift getCurrentTime
    let elapsed = diffUTCTime stop start
        status  = statusCode $ responseStatus resp
        msg = (show status) ++ " from " ++ url ++ " in " ++ (show elapsed)
    lift $ putStrLn msg

s :: String -> String
s = id

recache :: String -> IO ()
recache url = do
  manager <- newManager tlsManagerSettings
  recache' manager url

recache' :: Manager -> String -> IO ()
recache' manager url = do
  start <- getCurrentTime
  initReq <- parseUrl "http://api.prerender.io/recache"
  let params = object [ "prerenderToken" .= s "ZqGGqCmymhYXRRt46kWa"
                      , "url" .= url
                      ]
      req = initReq { method = "POST"
                    , requestHeaders = [ ("Content-Type", "application/json") ]
                    , requestBody = RequestBodyLBS (encode params) }
  runResourceT $ do
    resp <- http req manager
    stop <- lift getCurrentTime
    let elapsed = diffUTCTime stop start
        status  = statusCode $ responseStatus resp
        msg = (show status) ++ " from " ++ url ++ " in " ++ (show elapsed)
    lift $ putStrLn msg

crawlConcurrently :: (Manager -> String -> IO ()) -> Manager -> [String] -> IO ()
crawlConcurrently f manager urls = mapM_ crawlChunk chunks
  where
    chunks = chunksOf 100 urls
    crawlChunk = mapConcurrently go
    go arg = do
        handle (logFailure arg) $
          recoverAll (exponentialBackoff 1000000 <> limitRetries 3) $ \_ -> f manager arg

logFailure :: String -> SomeException -> IO ()
logFailure arg e = do
    appendFile "failed-urls" (arg ++ "\n")
    print e

main :: IO ()
main = do
  filename:_ <- getArgs
  urls <- readLines filename
  manager <- newManager tlsManagerSettings

  -- To prewarm the prerender cache, call the crawl function here.
  -- To recache, use recache'.  Prerender limits us to a few recache calls per
  -- minute, so if you have to update large numbers of URLs, you need to email
  -- prerender to get them to clear the cache and then use prewarm instead of
  -- recache.
  crawlConcurrently recache' manager urls

