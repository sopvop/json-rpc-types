{-# LANGUAGE OverloadedStrings #-}
-- | Method routing table and execution.
module Network.JsonRpc.Server
  ( MethodsTable
  , methodsTable
  , Method
  , method
  , methodAsync
  , executeRequest
  , runRequest
  , Message(..)
  , successMessage
  , errorMessage
  , customErrorMessage
  , customDetailedErrorMessage
  , jsonParseErrorMessage
  , ToResponse(..)
  , Success(..)
  -- * Handy reexports
  , module X
  )  where

import Control.Monad.IO.Class  (MonadIO, liftIO)
import Data.Aeson              (FromJSON, ToJSON, Value (Null), pairs, (.=))
import Data.Aeson              qualified as J
import Data.Aeson.Encoding     qualified as J
import Data.Aeson.Types        qualified as J
import Data.ByteString.Builder qualified as BL
import Data.ByteString.Lazy    qualified as LB
import Data.Foldable
import Data.HashMap.Strict     (HashMap)
import Data.HashMap.Strict     qualified as HashMap
import Data.Text               (Text)

import Network.JsonRpc.Errors
    (Error, invalidParams, methodNotFound, parseError)
import Network.JsonRpc.Types
    (MethodName (..), Request (..), RequestId (..), Response (..))

import Network.JsonRpc.Errors  qualified as X

import Network.JsonRpc.Types   qualified as X


-- | Handy alias for 'LB.ByteString'.
newtype Message = Message { messageLBS :: LB.ByteString }

errorMessage :: ToJSON a => Error a -> Message
errorMessage = Message . J.encode

customErrorMessage :: RequestId -> X.ErrorCode -> Text -> Message
customErrorMessage rid code msg =
  Message . BL.toLazyByteString $ J.fromEncoding $ pairs ("id" .= rid <> J.pair "error" err)
  where
    err = pairs $ "code" .= code <> "message" .= msg

customDetailedErrorMessage :: ToJSON a => Maybe RequestId -> X.ErrorCode -> Text -> a -> Message
customDetailedErrorMessage rid code msg dat =
  Message . BL.toLazyByteString $ J.fromEncoding $ pairs ("id" .= rid <> J.pair "error" err)
  where
    err = pairs $ "code" .= code <> "message" .= msg <> "data" .= dat

successMessage :: ToJSON a => RequestId -> a -> Message
successMessage rid v = Message . BL.toLazyByteString . J.fromEncoding $ pairs ("id" .= rid <> "result" .= v)

jsonParseErrorMessage :: String -> Message
jsonParseErrorMessage msg =
  Message . BL.toLazyByteString $ J.fromEncoding $ pairs ("id" .= Null <> J.pair "message" err)
  where
    err = pairs $ "message" .= msg



-- | Table of method handlers.
type MethodsTable c m = HashMap MethodName (Method c m)

-- | Method is just a request to response function.
type Method c m = (Message -> IO ()) -> c -> Request Value -> m ()

class ToResponse a where
  toResponse :: RequestId -> a -> Message

newtype Success a = Success a
  deriving stock (Eq, Show)

instance ToJSON a => ToResponse (Success a) where
  toResponse rid (Success v) = successMessage rid v

-- | Constructs 'MethodsTable'
methodsTable :: [(MethodName, Method c m)] -> MethodsTable c m
methodsTable = HashMap.fromList

-- | Construct a 'Method' from monadic function.
method
  :: FromJSON req
  => ToResponse resp
  => MonadIO m
  => Text
  -> (c -> req -> m resp)
  -> (MethodName, Method c m)
method nm m = methodAsync nm go
 where
   go sendMesg c req = do
     m c req >>= liftIO . sendMesg

methodAsync
  :: FromJSON req
  => ToResponse resp
  => MonadIO m
  => Text
  -> ((resp -> IO ()) -> con -> req -> m ())
  -> (MethodName, Method con m)
methodAsync nm m = (MethodName nm, go)
 where
   go sendMsg c req = case requestId req of
     Nothing -> pure () -- let user handle it?
     Just reqId -> case parseParams reqId $ requestParams req of
        Left e -> liftIO $ sendMsg (Message $ J.encode e)
        Right parsedReq -> do
           m send c parsedReq
     where
       send result = case requestId req of
         Nothing -> pure ()
         Just rid -> sendMsg $ toResponse rid result


-- | Parses method params from request
parseParams :: FromJSON a
            => RequestId
            -> Value
            -> Either (Response String ()) a
parseParams rid req = case J.parseEither J.parseJSON req of
  Left e -> Left $ invalidParams rid (Just e)
  Right r -> pure r

-- | A little helper
nothingAtAll :: Maybe ()
nothingAtAll = Nothing

-- | Decode and execute request.
runRequest
  :: MonadIO m
  => (Message -> IO ())
  -> c
  -> LB.ByteString
  -> MethodsTable c m
  -> m ()
runRequest send c lbs table = case decodeRequest lbs of
  Left e -> liftIO $ send e
  Right req -> executeRequest table send c req

-- | Execute parsed request
executeRequest
  :: MonadIO m
  => MethodsTable c m
  -> (Message -> IO ())
  -> c
  -> Request Value
  -> m ()
executeRequest table send c req =
  case findMethod table req of
    Left msg -> for_ msg $ liftIO . send
    Right f -> f send c req

-- | Decode request from json stream
decodeRequest :: FromJSON b => LB.ByteString -> Either Message b
decodeRequest lbs =
  case J.eitherDecode lbs of
    Left e -> Left $ jsonParseErrorMessage e
    Right req -> pure req

-- | Find method in methods table
findMethod :: MethodsTable c m
           -> Request Value
           -> Either (Maybe Message) (Method c m)
findMethod table req =
  case HashMap.lookup (requestMethod req) table of
    Nothing -> Left $ case requestId req of
      Nothing -> Nothing
      Just rid -> Just . Message $ J.encode (methodNotFound rid nothingAtAll :: Response () ())
    Just m -> Right m

