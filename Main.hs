{-|
Module      : TFTP
Description : A very simple TFTP client.
Copyright   : Jan Synáček, April 2016

Licence     : GPLv2
Maintainer  : jan.synacek@gmail.com

A very simple TFTP client.
-}
module Main where

import Prelude hiding (getContents, length, log)
import Control.Exception (throw)
import Control.Monad.RWS
import Data.Char (toLower)
import Data.ByteString.Lazy (ByteString(..), toStrict, fromStrict, length, hPut, hGet)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString hiding (recv)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import qualified System.IO as IO
import TFTP


data TFTPCommand = Get | Put deriving (Eq, Show)

data Options = Options
  { optCommand    :: TFTPCommand
  , optBlockSize  :: Int
  , optWindowSize :: Int
  , optVerbose    :: Bool
  , optPort       :: String
  , optHostname   :: String
  , optFiles      :: [FilePath]
  -- ^ Files to operate on. Currently, only the first one is used. There is always at least one.
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options { optCommand    = Get
                         , optBlockSize  = 512
                         , optWindowSize = 1
                         , optVerbose    = False
                         , optPort       = "69"
                         , optHostname   = "localhost"
                         , optFiles      = []
                         }

parseCommand :: String -> IO TFTPCommand
parseCommand cmd = case cmd' of
                    "get" -> return Get
                    "put" -> return Put
                    _     -> printErrLn ("Invalid command: " ++ cmd') >> dieUsage
  where cmd' = map toLower cmd

options :: [OptDescr (Options -> IO Options)]
options = [ Option ['c'] [] (ReqArg (\c opt -> do
                                        cmd <- parseCommand c
                                        return opt { optCommand = cmd }) "COMMAND")
            "TFTP command: get (default) or put"
          , Option ['b'] [] (ReqArg (\b opt -> return $ opt { optBlockSize = read b }) "BLOCKSIZE")
            "blksize TFTP option according to RFC2348"
          , Option ['w'] [] (ReqArg (\w opt -> return $ opt { optWindowSize = read w }) "WINDOWSIZE")
            "windowsize TFTP option according to RFC7440"
          , Option ['v'] [] (NoArg  (\  opt -> return $ opt { optVerbose = True }))
            "turn on verbose mode"
          , Option ['p'] [] (ReqArg (\p opt -> return $ opt { optPort = p }) "PORT")
            "port"
          ]

printErrLn :: String -> IO ()
printErrLn = IO.hPutStrLn IO.stderr

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  let uhdr = "usage: " ++ prog ++ " [-b blocksize][-w windowsize] <hostname> <filename> [<filename>, ...]"
  printErrLn $ usageInfo uhdr options

dieUsage :: IO a
dieUsage = printUsage >> exitFailure

parse :: [String] -> IO Options
parse args = do
  case getOpt Permute options args of
    (_, [], []) -> printErrLn "No hostname and filename(s) specified." >> dieUsage
    (_, (_:[]), []) -> printErrLn "No file(s) specified." >> dieUsage
    (optactions, rest, []) -> do
      o <- foldl (>>=) (return defaultOptions) optactions
      return $ o { optHostname = head rest, optFiles = tail rest }
    (_, _, errs) -> mapM_ printErrLn errs >> dieUsage

main :: IO ()
main = do
  opts <- parse =<< getArgs
  putStrLn $ show opts -- DEBUG
  (ai:_) <- getAddrInfo Nothing (Just $ optHostname opts) (Just $ optPort opts)
  sock <- socket (addrFamily ai) Datagram defaultProtocol
  let client = if optCommand opts == Get
               then runReceiver
               else runSender
  void $ execRWST client opts (Connection sock (addrAddress ai))


data Connection = Connection
  { conSocket  :: Socket
  , conAddress :: SockAddr
  }

type TFTPClient = RWST Options () Connection IO

log :: String -> TFTPClient ()
log msg = do
  opts <- ask
  liftIO $ when (optVerbose opts) $ putStr msg

logLn :: String -> TFTPClient ()
logLn msg = do
  opts <- ask
  liftIO $ when (optVerbose opts) $ putStrLn msg

logLn' :: (Show a) => a -> TFTPClient ()
logLn' msg = logLn $ show msg

throwServerError :: Message -> a
throwServerError msg = throw $ TFTPException $ "Server error: " ++ errStr msg
  where errStr (Error err) = show err
        errStr _ = ""

updateAddress :: SockAddr -> TFTPClient ()
updateAddress addr = do
  conn <- get
  put $ conn { conAddress = addr }

recv :: Socket -> Int -> TFTPClient Message
recv sock n = do
  (buf, from) <- liftIO $ recvFrom sock n
  let msg = decode $ fromStrict buf
  when (isErrorMsg msg) $ throwServerError msg
  updateAddress from
  return msg

runReceiver :: TFTPClient ()
runReceiver = do
  conn <- get
  opts <- ask
  let sock = conSocket conn
  let file = head $ optFiles opts

  log $ "Sending RRQ for '" ++ file ++ "'... "
  liftIO $ void $ sendTo sock (toStrict $ encode $ rrq file opts) (conAddress conn)
  logLn "Done."

  log "Receiving OACK... "
  msg <- recv sock 1024
  logLn' msg

  log "Sending ACK... "
  conn <- get
  size <- liftIO $ sendTo sock (toStrict $ encode $ ACK 0) (conAddress conn)
  logLn "Initial protocol completed."

  log "Receiving data... "
  recvData
  logLn "Done."

    where
      rrq file opts = RRQ file Octet $ makeTFTPOpts
                                       [ ("blksize", show $ optBlockSize opts)
                                       , ("windowsize", show $ optWindowSize opts)
                                       ]

recvData :: TFTPClient ()
recvData = do
  opts <- ask
  hnd  <- liftIO $ IO.openBinaryFile (head $ optFiles opts) IO.WriteMode
  loop hnd 1 1 (optBlockSize opts) (optWindowSize opts)
  liftIO $ IO.hClose hnd
    where
      loop :: IO.Handle -> Int -> Int -> Int -> Int -> TFTPClient ()
      loop hnd block window blockSize windowSize = do
        (bstr, go) <- recvBlock block window blockSize windowSize
        liftIO $ hPut hnd bstr
        when go $ loop hnd (block+1) (window+1) blockSize windowSize

recvBlock :: Int -> Int -> Int -> Int -> TFTPClient (ByteString, Bool)
recvBlock block window blockSize windowSize = do
  conn <- get
  let sock = conSocket conn
  msg <- recv sock (blockSize+4)
  conn <- get
  let addr = conAddress conn
  case msg of
    DATA block bstr ->
      if length bstr == fromIntegral blockSize
      then do
        when (window `mod` windowSize == 0) $ do
          liftIO $ void $ sendTo sock (toStrict $ encode $ ACK block) addr
        return (bstr, True)
      else do
        liftIO $ void $ sendTo sock (toStrict $ encode $ ACK block) addr
        return (bstr, False)
    Error err -> throwServerError msg
    _ -> throw $ TFTPException $ "Unexpected server message: " ++ show msg



runSender :: TFTPClient ()
runSender = do
  conn <- get
  opts <- ask
  let sock = conSocket conn
  let file = head $ optFiles opts

  log $ "Sending WRQ for '" ++ file ++ "'... "
  liftIO $ void $ sendTo sock (toStrict $ encode $ wrq file opts) (conAddress conn)
  logLn "Done."

  log "Receiving OACK... "
  msg <- recv sock 1024
  logLn' msg

  log "Sending data... "
  sendData
  logLn "Done."

    where
      wrq file opts = WRQ file Octet $ makeTFTPOpts
                                       [ ("blksize", show $ optBlockSize opts)
                                       , ("windowsize", show $ optWindowSize opts)
                                       ]

sendData :: TFTPClient ()
sendData = do
  opts <- ask
  hnd  <- liftIO $ IO.openBinaryFile (head $ optFiles opts) IO.ReadMode
  loop hnd 1 1 (optBlockSize opts) (optWindowSize opts)
  liftIO $ IO.hClose hnd
    where
      loop :: IO.Handle -> Int -> Int -> Int -> Int -> TFTPClient ()
      loop hnd block window blockSize windowSize = do
        bstr <- liftIO $ hGet hnd blockSize
        go <- sendBlock bstr block window blockSize windowSize
        when go $ loop hnd (block+1) (window+1) blockSize windowSize

sendBlock :: ByteString -> Int -> Int -> Int -> Int -> TFTPClient Bool
sendBlock bstr block window blockSize windowSize = do
  conn <- get
  let sock = conSocket conn
  size <- liftIO $ sendTo sock (toStrict $ encode $ DATA (fromIntegral block) bstr) (conAddress conn)
  if size == blockSize+4
    then if (window `mod` windowSize == 0)
         then do
              -- TODO: Errors and out-of-sync ACKs should be handled here,
              -- but our networks are solid, aren't they?
              void $ recv sock 1024
              return True
         else return True
    else do
         -- TODO: Again, errors.
         void $ recv sock 1024
         return False
