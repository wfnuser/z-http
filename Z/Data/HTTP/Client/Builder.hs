module Z.Data.HTTP.Client.Builder where

import Control.Monad.State
import GHC.Generics
import qualified Z.Data.ASCII as C
import qualified Z.Data.Builder as B
import Z.Data.CBytes
import Z.Data.HTTP.Client.Types
import qualified Z.Data.Parser as P
import qualified Z.Data.Text as T
import qualified Z.Data.Vector as V

data Request = Request
  { requestMethod :: !Method,
    requestPath :: !Path,
    requestVersion :: !HTTPVersion,
    requestHeaders :: Headers
  }
  deriving (Generic)
  deriving (T.Print)

emptyRequest :: Request
emptyRequest = Request V.empty V.empty HTTP1_1 V.empty

buildRequest :: State Request a -> Request
buildRequest s = execState s emptyRequest

setHeader :: (HeaderKey, HeaderValue) -> State Request ()
setHeader (k, v) = do
  q <- get
  put q {requestHeaders = V.snoc (requestHeaders q) (k, v)}

setMethod :: Method -> State Request ()
setMethod m = do
  q <- get
  put q {requestMethod = m}

setHost :: Host -> State Request ()
setHost (hostName, portNumber) = do
  q <- get
  put q {requestHeaders = V.snoc (requestHeaders q) ("Host", toBytes hostName)}

setPath :: Path -> State Request ()
setPath p = do
  q <- get
  put q {requestPath = p}

requestToBytes :: Request -> V.Bytes
requestToBytes req = mconcat [method, SPACE, path, SPACE, version, CRLF, headers, CRLF]
  where
    method :: V.Bytes = requestMethod req
    path :: V.Bytes = requestPath req
    version :: V.Bytes = versionToBytes $ requestVersion req
    headers :: V.Bytes = headersToBytes $ requestHeaders req
