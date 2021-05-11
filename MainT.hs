module MainT where

import Control.Monad.State
import Z.Data.HTTP.Client
import Z.Data.HTTP.Client.Common
import Z.Data.HTTP.Client.Dns
import Z.Data.HTTP.Client.Request
import Z.Data.HTTP.Client.Response
import qualified Z.Data.Parser as P
import qualified Z.Data.Text as T
import qualified Z.Data.Vector as V
import Z.IO.Buffered
import Z.IO.Network
import Z.IO.Resource

main :: IO ()
main = do
  res <- sendRequest $ buildRequest req
  mapM_ (print . T.validate . snd) $ V.unpack $ responseHeaders res
  where
    req = do
      setMethod "Get"
      setPath "/"
      setHost ("www.baidu.com", 80)

test :: IO ()
test = print $ requestToBytes req
  where
    (_, req) = runState (setMethod "GET" >> setHost ("www.bing.con", 80)) emptyRequest