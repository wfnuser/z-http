module MainT where

import Control.Monad.State
import Z.Data.HTTP.Client.Builder
import Z.Data.HTTP.Client.Dns
import Z.Data.HTTP.Client.Response
import Z.Data.HTTP.Client.Types
import qualified Z.Data.Parser as P
import qualified Z.Data.Text as T
import qualified Z.Data.Vector as V
import Z.IO.Buffered
import Z.IO.Network
import Z.IO.Resource

main :: IO ()
main = do
  addr <- resolveDNS ("www.baidu.com", 80)
  withResource (initTCPClient defaultTCPClientConfig {tcpRemoteAddr = addrAddress addr}) $ \tcp -> do
    (i, o) <- newBufferedIO tcp
    let req = do
          setMethod "GET"
          setPath "/"
          setHost ("www.baidu.com", 80)
    let reqB = requestToBytes $ buildRequest req
    print . T.validate $ reqB
    writeBuffer' o $ reqB
    -- writeBuffer' o "GET http://www.bing.com HTTP/1.1\r\nHost: www.bing.com\r\n\r\n"
    buf <- readBuffer i
    print . T.validate $ buf
    let res = P.parse responseParser buf
    case res of
      (_, Right res) -> mapM_ (print . T.validate . snd) (V.unpack (responseHeaders res))

test :: IO ()
test = print $ requestToBytes req
  where
    (_, req) = runState (setMethod "GET" >> setHost ("www.bing.con", 80)) emptyRequest