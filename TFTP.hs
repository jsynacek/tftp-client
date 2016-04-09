{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : TFTP
Description : Abstractions for the TFTP protocol.
Copyright   : Sven Heyll, 2012
              Jan Synáček, 2016

Licence     : GPLv2
Maintainer  : jan.synacek@gmail.com
Stability   : experimental
Portability : POSIX

Abstractions for the TFTP protocol (<https://tools.ietf.org/html/rfc1350 RFC1350>)
and its option extensions (<https://tools.ietf.org/html/rfc2347 RFC2347>).
This module is an improvement of the original implementation from
<https://hackage.haskell.org/package/tftp>.
-}
module TFTP
    ( Message(..)
    , Mode(..)
    , TFTPError(..)
    , TFTPException(..)
    , decode
    , encode
    , makeTFTPOpts
    , isErrorMsg
    ) where

import Control.Applicative
import Control.Exception (Exception)
import Control.Monad
import Data.Char
import Data.Word
import Data.Binary
import Data.Binary.Get (getLazyByteStringNul, getRemainingLazyByteString, isEmpty)
import Data.Binary.Put (PutM)
import Data.ByteString.Lazy
import Data.Typeable (Typeable)


data TFTPException = TFTPException String
  deriving Typeable

instance Exception TFTPException
instance Show TFTPException where
  showsPrec _ (TFTPException err) = showString err

type TFTPOptions = [(NString, NString)]

data Message = RRQ String Mode TFTPOptions |
               -- ^ Read request
               WRQ String Mode TFTPOptions |
               -- ^ Write request
               DATA BlockNumber ByteString |
               -- ^ Data block with a raw bytestring
               ACK BlockNumber |
               -- ^ Acknowledge message
               Error TFTPError |
               -- ^ Error message
               OACK TFTPOptions
               -- ^ Acknowledge options
          deriving(Read, Show, Ord, Eq)

putOpts :: TFTPOptions -> PutM ()
putOpts = mapM_ $ \(o, v) -> put o >> put v

makeTFTPOpts :: [(String, String)] -> TFTPOptions
makeTFTPOpts preopts = fmap wrap preopts
  where wrap (l, r) = (NString l, NString r)

instance Binary Message where
    put (RRQ fname mode opts) = do
                          put (1 :: Word16)
                          put (NString fname)
                          put mode
                          putOpts opts


    put (WRQ fname mode opts) = do
                          put (2 :: Word16)
                          put (NString fname)
                          put mode
                          putOpts opts

    put (DATA blockIndex chunk) = do
                          put (3 :: Word16)
                          put blockIndex
                          put (DC chunk)

    put (ACK blockIndex) = do
                          put (4 :: Word16)
                          put blockIndex

    put (Error err)      = do
                          put (5 :: Word16)
                          put err

    put (OACK opts) = do
                          put (6 :: Word16)
                          putOpts opts

    get = do
      opcode <- get :: Get Word16
      case opcode of
        1 -> do
            NString fname <- get
            mode <- get
            opts <- getOpts
            return $ RRQ fname mode opts

        2 -> do
            NString fname <- get
            mode <- get
            opts <- getOpts
            return $ WRQ fname mode opts

        3 -> DATA <$> get <*> (unDC <$> get)

        4 -> ACK <$> get

        5 -> Error <$> get

        6 -> OACK <$> getOpts

        where
            getOpts = loop []
            loop acc = do
                opt <- get
                val <- get
                empty <- isEmpty
                if not empty
                    then loop $ (opt, val):acc
                    else return $ (opt, val):acc

-- | The data type for block numbers in `Message's
type BlockNumber = Word16

-- | The data mode to encode the data with
data Mode =
    NetASCII |
    -- ^ "netascii" mode
    Octet
    -- ^ "octet" mode
          deriving(Read, Show, Ord, Eq)

instance Binary Mode where
    put NetASCII = put $ NString "netascii"
    put Octet = put $ NString "octet"

    get = do
      NString str <- get
      return $ case toLower <$> str of
                 "netascii" -> NetASCII
                 "octet" -> Octet


-- | The error codes as defined in the RFC 1350
data TFTPError = ErrorMessage String |
                 -- ^ Encapsulates a custom message for a non-standard error
                 FileNotFound |
                 AccessViolation |
                 DiskFull |
                 IllegalTFTPOperation |
                 UnknownTransferID |
                 FileAlreadyExists |
                 NoSuchUser |
                 OptionNegotiation
          deriving(Read, Show, Ord, Eq)

getErrorCode :: TFTPError -> Word16
getErrorCode (ErrorMessage _str) = 0
getErrorCode FileNotFound = 1
getErrorCode AccessViolation = 2
getErrorCode DiskFull = 3
getErrorCode IllegalTFTPOperation = 4
getErrorCode UnknownTransferID = 5
getErrorCode FileAlreadyExists = 6
getErrorCode NoSuchUser = 7
getErrorCode OptionNegotiation = 8

getErrorMsg :: TFTPError -> NString
getErrorMsg (ErrorMessage str) = NString str
getErrorMsg _ = NString ""

makeTFTPError :: Word16 -> NString -> TFTPError
makeTFTPError 0 (NString msg) = ErrorMessage msg
makeTFTPError 1 _msg = FileNotFound
makeTFTPError 2 _msg = AccessViolation
makeTFTPError 3 _msg = DiskFull
makeTFTPError 4 _msg = IllegalTFTPOperation
makeTFTPError 5 _msg = UnknownTransferID
makeTFTPError 6 _msg = FileAlreadyExists
makeTFTPError 7 _msg = NoSuchUser
makeTFTPError 8 _msg = OptionNegotiation

isErrorMsg :: Message -> Bool
isErrorMsg (Error _) = True
isErrorMsg _ = False

instance Binary TFTPError where
    put err = put (getErrorCode err) *> put (getErrorMsg err)
    get = makeTFTPError <$> get <*> get

newtype NString = NString String
    deriving (Read, Ord, Eq)

data DataChunk = DC { unDC :: ByteString }

instance Binary DataChunk where
    put (DC bs) = mapM_ put (unpack bs)
    get = DC <$> getRemainingLazyByteString

instance Show NString where
    show (NString s) = s

instance Binary NString where
    put (NString str) = forM_ str put >> put ('\NUL':: Char)
    get = pure bsToNString <*> getLazyByteStringNul
        where
          bsToNString = NString . ((toEnum . fromIntegral) <$>) . unpack
