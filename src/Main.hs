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
import Network.HTTP.Conduit (parseUrl, http, Request(..), Response(..), Manager, tlsManagerSettings, newManager)
import Network.HTTP.Types.Status (Status(..))
import System.Environment   (getArgs)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.Async (mapConcurrently)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List.Split (chunksOf)
import Control.Exception.Extra (retry)



readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

crawl manager url = do
  start <- getCurrentTime
  initReq <- parseUrl url
  let req = initReq {
              requestHeaders = [("User-Agent", "Googlebot/2.1 (+http://www.google.com/bot.html)")]
            }
  runResourceT $ do
    resp <- http req manager
    stop <- lift getCurrentTime
    lift $ putStrLn $ "got status " ++ (show $ statusCode $ responseStatus resp) ++ " from " ++ url ++ " in " ++ (show $ diffUTCTime stop start)

crawlConcurrently manager urls = do
  mapM_ (mapConcurrently $ retry 3 . crawl manager) (chunksOf 100 urls)

main = do
  filename:_ <- getArgs
  urls <- readLines filename
  manager <- newManager tlsManagerSettings
  crawlConcurrently manager urls

