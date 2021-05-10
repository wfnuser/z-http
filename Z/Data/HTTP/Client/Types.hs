{-# LANGUAGE LambdaCase #-}

module Z.Data.HTTP.Client.Types where

import GHC.Generics
import qualified Z.Data.Text as T
import qualified Z.Data.Vector as V
import Z.IO.Network

type Host = (HostName, PortNumber)

type Method = V.Bytes

type Path = V.Bytes

data HTTPVersion = HTTP1_1
  deriving (Generic)
  deriving (T.Print)

versionToBytes :: HTTPVersion -> V.Bytes
versionToBytes = \case
  HTTP1_1 -> "HTTP/1.1"

type Headers = (V.Vector (HeaderKey, HeaderValue))

headersToBytes :: Headers -> V.Bytes
headersToBytes headers = case V.unpack headers of
  [] -> ""
  (hk, hv) : hs -> mconcat [hk, SPACE, hv, SPACE, CRLF] <> headersToBytes (V.pack hs)

type HeaderKey = V.Bytes

type HeaderValue = V.Bytes

pattern CRLF :: V.Bytes
pattern CRLF = "\r\n"

pattern SPACE :: V.Bytes
pattern SPACE = " "