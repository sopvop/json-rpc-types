{-# LANGUAGE OverloadedStrings #-}
-- | Method routing table and execution.
module Network.JsonRpc.Server
    ( MethodsTable
    , methodsTable
    , Method
    , method
    , executeRequest
    , runRequest
    , Message
    -- * Handy reexports
    , module X
    )  where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Lazy as LB
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)

import           Network.JsonRpc.Errors
    (Error, invalidParams, methodNotFound, parseError)
import           Network.JsonRpc.Types
    (MethodName (..), Request (..), RequestId (..), Response (..))

import qualified Network.JsonRpc.Errors as X

import qualified Network.JsonRpc.Types as X


-- | Handy alias for 'LB.ByteString'.
type Message = LB.ByteString

-- | Table of method handlers.
type MethodsTable c m = HashMap MethodName (Method c m)

-- | Method is just a request to response function.
type Method c m = (Message -> IO ()) -> c -> Request Value -> m ()

-- | Constructs 'MethodsTable'
methodsTable :: [(MethodName, Method c m)] -> MethodsTable c m
methodsTable = HashMap.fromList

-- | Construct a 'Method' from monadic function.
method
  :: FromJSON req
  => ToJSON err
  => ToJSON resp
  => MonadIO m
  => Text
  -> (c -> req -> m (Either (Error err) resp))
  -> (MethodName, Method c m)
method nm m = methodAsync nm go
 where
   go sendMesg c req = do
     m c req >>= liftIO . sendMesg

methodAsync
  :: FromJSON req
  => ToJSON err
  => ToJSON resp
  => MonadIO m
  => Text
  -> ((Either (Error err) resp -> IO ()) -> con -> req -> m ())
  -> (MethodName, Method con m)
methodAsync nm m = (MethodName nm, go)
 where
   go sendMsg c req = case parseParams req of
        Left e -> liftIO $ sendMsg (J.encode e)
        Right parsedReq -> do
           m send c parsedReq
     where
       send result = case requestId req of
         Nothing -> pure ()
         Just rid -> sendMsg . J.encode $ Response rid (Just result)


-- | Parses method params from request
parseParams :: FromJSON a
            => Request Value
            -> Either (Response String ()) a
parseParams req = case J.parseEither J.parseJSON reqNull of
                Left e -> Left $ invalidParams rid (Just e)
                Right r -> pure r
  where
    rid = fromMaybe (RequestIdString "") (requestId req)
    reqNull = fromMaybe J.Null (requestParams req)

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
     Left e -> Left $ J.encode (parseError (Just e) :: Response String ())
     Right req -> pure req

-- | Find method in methods table
findMethod :: MethodsTable c m
           -> Request Value
           -> Either (Maybe Message) (Method c m)
findMethod table req =
    case HashMap.lookup (requestMethod req) table of
      Nothing -> Left $ case requestId req of
        Nothing -> Nothing
        Just rid -> Just $ J.encode (methodNotFound rid nothingAtAll :: Response () ())
      Just m -> Right m

