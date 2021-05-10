module Z.Data.HTTP.Client.Response where

import Data.Word
import qualified Z.Data.ASCII as C
import Z.Data.HTTP.Client.Types
import qualified Z.Data.Parser as P
import qualified Z.Data.Text as T
import qualified Z.Data.Vector as V

testResponse :: V.Bytes
testResponse = "HTTP/1.1 200 OK\r\nDate: Sat, 09 Oct 2010 14:28:02 GMT\r\nServer: Apache\r\nLast-Modified: Tue, 01 Dec 2009 20:18:22 GMT\r\nETag: \"51142bc1-7449-479b075b2891b\"\r\nAccept-Ranges: bytes\r\nContent-Length: 29769\r\nContent-Type: text/html\r\n\r\n<!DOCTYPE html... (here comes the 29769 bytes of the requested web page)"

data Response = Response
  { responseVersion :: !HTTPVersion,
    responseCode :: !Word16,
    responseMessage :: !V.Bytes,
    responseHeaders :: Headers
  }
  deriving (Show)

responseParser :: P.Parser Response
responseParser = do
  P.bytes "HTTP/"
  m <- P.satisfy C.isDigit
  P.word8 C.DOT
  n <- P.satisfy C.isDigit
  P.skipSpaces
  a <- P.satisfy C.isDigit
  b <- P.satisfy C.isDigit
  c <- P.satisfy C.isDigit
  P.skipSpaces
  let code = toInt a * 100 + toInt b * 10 + toInt c

  msg <- P.takeWhile (/= C.CARRIAGE_RETURN)
  P.word8 C.CARRIAGE_RETURN
  P.word8 C.NEWLINE

  let loopHeaders (acc :: Headers) = do
        m <- P.peekMaybe
        case m of
          Nothing -> pure acc
          Just C.CARRIAGE_RETURN -> do
            P.word8 C.CARRIAGE_RETURN
            P.word8 C.NEWLINE
            pure acc
          Just c -> do
            h <- P.takeWhile (/= C.COLON)
            P.word8 C.COLON
            P.skipSpaces
            k <- P.takeWhile (/= C.CARRIAGE_RETURN)
            P.word8 C.CARRIAGE_RETURN
            P.word8 C.NEWLINE
            loopHeaders $ V.cons (h, k) acc
  headers <- loopHeaders V.empty

  return $ Response HTTP1_1 code msg headers
  where
    toInt a = fromIntegral $ a - C.DIGIT_0
