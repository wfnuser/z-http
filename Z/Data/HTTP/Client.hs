module Z.Data.HTTP.Client where

import Z.Data.HTTP.Client.Dns
import Z.Data.HTTP.Client.Request
import Z.Data.HTTP.Client.Response
import qualified Z.Data.Parser as P
import Z.IO
import Z.IO.Buffered
import Z.IO.Network

sendRequest :: Request -> IO Response
sendRequest req = do
  addr <- resolveDNS ("www.baidu.com", 80)
  withResource (initTCPClient defaultTCPClientConfig {tcpRemoteAddr = addrAddress addr}) $ \tcp -> do
    (i, o) <- newBufferedIO tcp
    let reqB = requestToBytes req
    writeBuffer' o reqB
    buf <- readBuffer i
    let res = P.parse responseParser buf
    case res of
      (_, Right res) -> pure res
