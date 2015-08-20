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
import Data.Time                    (getCurrentTime
                                    ,diffUTCTime)
import Network.HTTP.Conduit         (parseUrl
                                    ,http
                                    ,Request(..)
                                    ,Response(..)
                                    ,Manager
                                    ,tlsManagerSettings
                                    ,newManager)
import Network.HTTP.Types.Status    (Status(..))
import System.Environment           (getArgs)


readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

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

crawlConcurrently manager urls = do
  mapM_ (mapConcurrently $ retry 3 . crawl manager) (chunksOf 100 urls)

main = do
  filename:_ <- getArgs
  urls <- readLines filename
  manager <- newManager tlsManagerSettings
  crawlConcurrently manager urls

