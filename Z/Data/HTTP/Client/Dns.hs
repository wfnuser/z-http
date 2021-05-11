module Z.Data.HTTP.Client.Dns where

import qualified Z.Data.Builder as B
import Z.Data.CBytes
import Z.Data.HTTP.Client.Common
import Z.IO.Network

resolveDNS :: Host -> IO AddrInfo
resolveDNS (hostName, portNumber) = head <$> getAddrInfo Nothing hostName (buildCBytes . B.int $ portNumber)