-- {-# ANN module ("HLint: ignore Avoid lambda"::String) #-}
-- {-# ANN module ("HLint: ignore Eta reduce"::String) #-}
-- {-# ANN module ("HLint: ignore Use /="::String) #-}
{-# LANGUAGE BangPatterns #-}

module HelloWorld where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad
import System.Random

-- data Async a =
--   Async ThreadId
--         (TMVar (Either SomeException a))
--
-- async :: IO a -> IO (Async a)
-- async action = do
--   var <- newEmptyTMVarIO
--   tId <- forkFinally action (atomically . putTMVar var)
--   return (Async tId var)
--
-- wait :: Async a -> IO a
-- wait = atomically . waitSTM
--
-- waitSTM :: Async a -> STM a
-- waitSTM a = do
--   r <- waitCatchSTM a
--   case r of
--     Left e -> throwSTM e
--     Right a -> return a
--
-- waitCatchSTM :: Async a -> STM (Either SomeException a)
-- waitCatchSTM (Async _ var) = readTMVar var
--
-- cancel :: Async a -> IO ()
-- cancel (Async t var) = throwTo t ThreadKilled
--
-- withAsync :: IO a -> (Async a -> IO b) -> IO b
-- withAsync io operation = bracket (async io) cancel operation
--
-- waitEither :: Async a -> Async b -> IO (Either a b)
-- waitEither a b =
--   atomically $ fmap Left (waitSTM a) `orElse` fmap Right (waitSTM b)
--
-- race :: IO a -> IO b -> IO (Either a b)
-- race ioa iob = withAsync ioa $ \a -> withAsync iob $ \b -> waitEither a b
--
-- timeout :: Int -> IO a -> IO (Maybe a)
-- timeout n m
--   | n <= 0 = error "timeout must be greater than zero"
--   | otherwise = do
--     r <- race (threadDelay $ secondsToMicroseconds n) m
--     case r of
--       Left _ -> return Nothing
--       Right a -> return (Just a)
-- run :: IO ()
-- run = do
--   direct <- async (getUrl "http://direct.ru")
--   adwords <- async (getUrl "http://adwords.com")
--   d <- wait direct
--   a <- wait adwords
--   print $ d ++ ", " ++ a
--   where
--     getUrl url = do
--       delay <- randomRIO (1, 3)
--       threadDelay $ secondsToMicroseconds delay
--       print $ url ++ " finished"
--       return $ "url " ++ url ++ " loaded in " ++ show delay ++ " seconds"
--
-- -- "http://direct.ru finished"
-- -- "http://adwords.com finished"
-- -- "url http://direct.ru loaded in 1 seconds, url http://adwords.com loaded in 2 seconds"
-- run :: IO ()
-- run = do
--   r <-
--     tryAny $
--     withAsync (getUrl "http://direct.ru") $ \direct ->
--       withAsync (getUrl "http://adwords.com") $ \adwords -> do
--         d <- wait direct
--         a <- wait adwords
--         print $ d ++ ", " ++ a
--   case r of
--     (Left _) -> print "error"
--     (Right _) -> print "ok"
--   threadDelay $ secondsToMicroseconds 20
--   where
--     getUrl url
--       | url == "http://direct.ru" = error "direct is down"
--       | otherwise = do
--         delay <- randomRIO (2, 3)
--         threadDelay $ secondsToMicroseconds delay
--         print $ url ++ " finished"
--         return $ "url " ++ url ++ " loaded in " ++ show delay ++ " seconds"
--     tryAny :: IO a -> IO (Either SomeException a)
--     tryAny = try
-- run :: IO ()
-- run = do
--   r <- race (getUrl "http://direct.ru") (getUrl "http://adwords.com")
--   print r
--   threadDelay $ secondsToMicroseconds 20
--   where
--     getUrl url = do
--       delay <- randomRIO (1, 3)
--       threadDelay $ secondsToMicroseconds delay
--       print $ url ++ " finished"
--       return $ "url " ++ url ++ " loaded in " ++ show delay ++ " seconds"
-- run :: IO ()
-- run = do
--   r <-
--     timeout 1 (race (getUrl "http://direct.ru") (getUrl "http://adwords.com"))
--   case r of
--     Nothing -> print "timed out"
--     (Just _) -> print "ok"
--   threadDelay $ secondsToMicroseconds 20
--   where
--     getUrl url = do
--       delay <- randomRIO (1, 2)
--       threadDelay $ secondsToMicroseconds delay
--       print $ url ++ " finished"
--       return $ "url " ++ url ++ " loaded in " ++ show delay ++ " seconds"
-- "timed out"
-- "http://direct.ru finished"
-- "ok"
-- "http://direct.ru finished"
-- Left "url http://direct.ru loaded in 2 seconds"
run :: IO ()
run = do
  c <- newBroadcastTChanIO
  f <- atomically $ dupTChan c
  s <- atomically $ dupTChan c
  forkIO $ forever $ printMessages "first" f
  forkIO $ forever $ printMessages "second" s
  loop c 0
  where
    loop ch i = do
      atomically $ writeTChan ch i
      threadDelay $ secondsToMicroseconds 1
      loop ch (i + 1)
    printMessages name ch = do
      m <- atomically $ readTChan ch
      print $ name ++ " " ++ show m
--
-- secondsToMicroseconds :: Int -> Int
-- secondsToMicroseconds = (* 1000000)
data TChan a

newTChan :: STM (TChan a)
newBroadcastTChan :: STM (TChan a)
writeTChan :: TChan a -> a -> STM ()
readTChan :: TChan a -> STM a
dupTChan :: TChan a -> STM (TChan a)

-- data MVar a
--
-- newEmptyMVar :: IO (MVar a)
-- newMVar :: a -> IO (MVar a)
-- takeMVar :: MVar a -> IO a
-- putMVar :: MVar a -> a -> IO ()
-- run :: IO ()
-- run = do
--   forkIO $ startPrinting "first" 0
--   forkIO $ startPrinting "second" 0
--   threadDelay 1000000000
--   where
--     startPrinting name counter =
--       forever $ do
--         id <- myThreadId
--         let !m = name ++ ", " ++ show counter ++ ", " ++ show id
--         print m
--         startPrinting name (counter + 1)
