module MainT where

import Control.Monad.State
import Z.Data.HTTP.Client.Builder
import Z.Data.HTTP.Client.Dns
import Z.Data.HTTP.Client.Types
import qualified Z.Data.Text as T
import Z.IO.Buffered (newBufferedIO, readBuffer, writeBuffer')
import Z.IO.Network
import Z.IO.Resource

main :: IO ()
main = do
  addr <- resolveDNS ("www.bing.com", 80)
  withResource (initTCPClient defaultTCPClientConfig {tcpRemoteAddr = addrAddress addr}) $ \tcp -> do
    (i, o) <- newBufferedIO tcp
    let req = requestToBytes (execState (setMethod "GET" >> setHost ("www.bing.com", 80)) emptyRequest)
    print . T.validate $ req
    writeBuffer' o $ req
    -- writeBuffer' o "GET http://www.bing.com HTTP/1.1\r\nHost: www.bing.com\r\n\r\n"
    readBuffer i >>= print . T.validate

test :: IO ()
test = print $ requestToBytes req
  where
    (_, req) = runState (setMethod "GET" >> setHost ("www.bing.con", 80)) emptyRequest