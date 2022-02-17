{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Binary.Builder      (Builder, fromByteString, putWord32be,
                                           singleton)
import           Data.Binary.Get          (Decoder (..), getByteString, getInt8,
                                           getWord32be, pushChunk,
                                           runGetIncremental)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC
import qualified Data.ByteString.Lazy     as BSL
import           Data.CaseInsensitive     (CI)
import qualified Data.CaseInsensitive     as CI
import           Data.IORef
import           Data.Maybe               (fromMaybe)
import           Data.ProtoLens.Encoding  (decodeMessage, encodeMessage)
import           Data.ProtoLens.Message   (Message, defMessage)
import           Lens.Micro
import           Network.HTTP.Types       (status200)
import           Network.HTTP2.Server     (NextTrailersMaker (..))
import           Network.Wai
import           Network.Wai.Handler.Warp (defaultHTTP2Data, http2dataTrailers,
                                           modifyHTTP2Data)
import qualified Network.Wai.Handler.Warp as Warp

import           Proto.Helloworld
import           Proto.Helloworld_Fields

main :: IO ()
main = do
  let warpSettings = Warp.defaultSettings & Warp.setPort 50005
                                          & Warp.setHost "127.0.0.1"
  putStrLn "listening on 50005"
  Warp.runSettings warpSettings fakeGrpcApp

fakeGrpcApp :: Application
fakeGrpcApp req rep = do
    r <- newIORef []

    modifyHTTP2Data req $ \h2data ->
      Just $! (fromMaybe defaultHTTP2Data h2data) { http2dataTrailers = trailersMaker r }

    rep $ responseStream status200 hdrs200 $ \write flush -> do
      handleRequestChunksLoop decoder handleEof nextChunk $ \rest msg ->
        if BSC.null rest
           then do (write . encode =<< handleHello msg) >> flush
           else error "left-overs"
      modifyGRPCStatus r req ("0", "WAI handler ended.")
    where
      hdrs200 =
        [ ("content-type", grpcContentTypeHV)
        , ("grpc-encoding", "identity")
        , ("trailer", CI.original grpcStatusH)
        , ("trailer", CI.original grpcMessageH)
        ]
      trailersMaker r Nothing = Trailers <$> readIORef r
      trailersMaker r _       = return $ NextTrailersMaker (trailersMaker r)
      handleEof = error "xx"
      nextChunk = BSL.toStrict <$> strictRequestBody req

handleHello :: HelloRequest -> IO HelloReply
handleHello input = return $ defMessage & message .~ (input ^. name)

modifyGRPCStatus :: IORef [(CI BSC.ByteString, BSC.ByteString)]
                 -> Request
                 -> (BSC.ByteString, BSC.ByteString)
                 -> IO ()
modifyGRPCStatus ref _ (s, msg) = writeIORef ref trailers
  where
    !trailers = if BSC.null msg then [status'] else [status', msg']
    status' = (grpcStatusH, s)
    msg' = (grpcMessageH, msg)

-- | Helpers to consume input in chunks.
handleRequestChunksLoop
  :: Decoder (Either String a)
  -- ^ Message decoder.
  -> IO b
  -- ^ Handler for handling end-of-streams.
  -> IO BS.ByteString
  -- ^ Action to retrieve the next chunk.
  -> (BS.ByteString -> a -> IO b)
  -- ^ Handler for a single message.
  -- The ByteString corresponds to leftover data.
  -> IO b
handleRequestChunksLoop dec handleEof nextChunk handleMsg =
  case dec of
    (Done unusedDat _ (Right val)) -> handleMsg unusedDat val
    (Done _ _ (Left err)) -> error $ "done-error: " ++ err
    (Fail _ _ err)        -> error $ "fail-error: " ++ err
    partial@(Partial _)   -> do
      chunk <- nextChunk
      if BS.null chunk
         then handleEof
         else handleRequestChunksLoop (pushChunk partial chunk) handleEof nextChunk handleMsg
{-# INLINEABLE handleRequestChunksLoop #-}

grpcStatusH :: CI BSC.ByteString
grpcStatusH = "grpc-status"

grpcMessageH :: CI BSC.ByteString
grpcMessageH = "grpc-message"

grpcContentTypeHV :: BSC.ByteString
grpcContentTypeHV = "application/grpc+proto"

decoder :: Message a => Decoder (Either String a)
decoder = runGetIncremental $ do
    _isCompressed <- getInt8      -- 1byte
    n <- getWord32be              -- 4bytes
    decodeMessage <$> getByteString (fromIntegral n)

encode :: Message m => m -> Builder
encode plain =
    mconcat [ singleton 0
            , putWord32be (fromIntegral $ BSC.length bin)
            , fromByteString bin
            ]
  where
    bin = encodeMessage plain
