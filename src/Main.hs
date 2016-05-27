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
import Control.Exception.Extra      (retry)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Class    (lift)
import Data.List.Split              (chunksOf)
import Data.String.Conv
import Data.Time                    (getCurrentTime
                                    ,diffUTCTime)
import Network.HTTP.Conduit         (parseUrl
                                    ,http
                                    ,Request(..)
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

recache :: Manager -> String -> IO ()
recache manager url = do
  start <- getCurrentTime
  initReq <- parseUrl "http://api.prerender.io/recache"
  let params = [("prerenderToker", Just "ZqGGqCmymhYXRRt46kWa")
               ,("url", Just $ toS url)
               ]
      req = setQueryString params initReq
  runResourceT $ do
    resp <- http req manager
    stop <- lift getCurrentTime
    let elapsed = diffUTCTime stop start
        status  = statusCode $ responseStatus resp
        msg = (show status) ++ " from " ++ url ++ " in " ++ (show elapsed)
    lift $ putStrLn msg

crawlConcurrently :: Manager -> [String] -> IO ()
crawlConcurrently manager urls = mapM_ crawlChunk chunks
  where chunks = chunksOf 100 urls
        crawlChunk = mapConcurrently $ retry 3 . crawl manager

main :: IO ()
main = do
  filename:_ <- getArgs
  urls <- readLines filename
  manager <- newManager tlsManagerSettings
  crawlConcurrently manager urls

