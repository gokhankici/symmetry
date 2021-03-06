module TestMain where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Transport hiding (send)

mainDelay = 3

mytest :: Process () -> IO ()
mytest s = do
  res <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  case res of
    Left exp -> print "Transport already exists!"
    Right t  -> do node <- newLocalNode t initRemoteTable
                   p    <- forkProcess node s
                   -- A 1 second wait. Otherwise the main thread can terminate before
                   -- our messages reach the logging process or get flushed to stdio
                   liftIO $ threadDelay (mainDelay*1000000)
                   closeTransport t
                   return ()

wait  :: Int -> Process ()
wait n = liftIO $ threadDelay (n*1000000)
-- ######################################################################

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg

testProcess :: Process ()
testProcess =  do
  -- Spawn another worker on the local node
  echoPid <- spawnLocal $ forever $ do
    -- Test our matches in order against each message in the queue
    receiveWait [match logMessage, match replyBack]

  -- The `say` function sends a message to a process registered as "logger".
  -- By default, this process simply loops through its mailbox and sends
  -- any received log message strings it finds to stderr.

  say "send some messages!"
  send echoPid "hello"
  self <- getSelfPid
  send echoPid (self, "hello")

  -- `expectTimeout` waits for a message or times out after "delay"
  m <- expectTimeout 1000000
  case m of
    -- Die immediately - throws a ProcessExitException with the given reason.
    Nothing  -> die "nothing came back!"
    (Just s) -> say $ "got " ++ s ++ " back!"
  return ()

