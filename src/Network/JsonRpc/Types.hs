-- | Module defining core types used in json-rpc
module Network.JsonRpc.Types
  ( RequestId (..)
  , MethodName (..)
  , Request (..)
  , Response (..)
  , Notification (..)
  , ErrorCode (..)
  , Error (..)
  ) where

import Data.Aeson
    (FromJSON (..), ToJSON (..), Value (Null, String), object, pairs,
    withObject, (.:), (.:?), (.=))
import Data.Aeson.Types qualified as J
import Data.Hashable    (Hashable (..))
import Data.Int         (Int64)
import Data.Maybe       (catMaybes)
import Data.Text        (Text)
import GHC.Generics     (Generic)

-- | Request id, either Int53 or string
data RequestId
  = RequestIdInt {-# UNPACK #-} !Int64
  | RequestIdString {-# UNPACK #-} !Text
  deriving stock (Show, Eq, Ord)

instance Hashable RequestId where
  hashWithSalt salt (RequestIdInt i) =
      salt `hashWithSalt` (0::Int) `hashWithSalt` i
  hashWithSalt salt (RequestIdString s) =
      salt `hashWithSalt` (1::Int) `hashWithSalt` s

instance FromJSON RequestId where
  parseJSON v = case v of
    String t -> pure (RequestIdString t)
    o        -> RequestIdInt <$> parseJSON o
  {-# INLINEABLE parseJSON #-}

instance ToJSON RequestId where
  toJSON r = case r of
    RequestIdInt !i -> toJSON i
    RequestIdString !t-> toJSON t
  toEncoding r = case r of
    RequestIdInt !i -> toEncoding i
    RequestIdString !t-> toEncoding t
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

-- | A name of a called method
newtype MethodName = MethodName { unMethodName :: Text }
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON, Hashable)


-- | A serialised function call comming from client.
data Request a = Request
  { requestId     :: !(Maybe RequestId)
  , requestMethod :: !MethodName
  , requestParams :: !a
  } deriving stock (Show, Generic)

instance FromJSON a => FromJSON (Request a) where
  parseJSON = withObject "json-rpc call" $ \o ->
    Request <$> o .:? "id"
            <*> o .: "method"
            <*> parseParams o
    where
      parseParams o = do
        r <- o .:? "params"
        case r of
          Nothing -> J.parserCatchError (parseJSON Null) $ \path msg ->
            J.parserThrowError (path <> [J.Key "params"]) msg
          Just v -> pure v

instance ToJSON a => ToJSON (Request a) where
  toJSON (Request cid m ps) =
      object $ ("jsonrpc", String "2.0")
        : catMaybes [ ("id" .=) <$> cid
                    , Just ("method" .= m)
                    , Just ("params" .= ps) ]

  toEncoding (Request cid m ps) =
      pairs $ "jsonrpc" .= ("2.0" :: Text)
           <> maybe mempty ("id" .=) cid
           <> "method" .= m
           <> "params" .= ps

  {-# INLINABLE toEncoding #-}
  {-# INLINABLE toJSON #-}

-- | Notification is like a 'Request' but without an id. Must not
-- return any result.
data Notification a = Notification
    { notifMethod :: !MethodName
    , notifParams :: !a
    } deriving stock (Show)

instance ToJSON a => ToJSON (Notification a) where
  toJSON (Notification meth params) =
    object [("jsonrpc", String "2.0")
           ,("method", String (unMethodName meth))
           ,"params" .= params]
  toEncoding (Notification meth params) =
    pairs $ "jsonrpc" .= ("2.0"::Text)
          <> "method" .= meth
          <> "params" .= params
  {-# INLINABLE toEncoding #-}
  {-# INLINABLE toJSON #-}

-- | A result of successeful function call returned to client
data Response e a = Response
  { responseId     :: !RequestId
  , responseResult :: !(Maybe (Either (Error e) a))
  } deriving stock (Show)

instance (ToJSON e, ToJSON a) => ToJSON (Response e a) where
  toJSON (Response !cid !dat) = object
    $ ("jsonrpc", String "2.0")
      : ("id" .= cid)
      : maybe [] (\(!v) -> [eitherER v] ) dat
    where
      eitherER = either ("error" .=) ("result" .=)
  toEncoding (Response !cid !dat) =
      pairs $ "jsonrpc" .= ("2.0" :: Text)
           <> "id" .= cid
           <> maybe mempty eitherER dat
    where
      eitherER = either ("error" .=) ("result" .=)

  {-# INLINABLE toEncoding #-}
  {-# INLINABLE toJSON #-}

instance (FromJSON e, FromJSON a) => FromJSON (Response e a) where
  parseJSON = withObject "jsonrpc-response" $ \o -> do
    -- ignore version for now
    rid <- o .: "id"
    err <- o .:? "error"
    case err of
      Nothing -> do
        result <- o .:? "result"
        pure $ Response rid (Right <$> result)
      Just e ->
        pure $ Response rid (Just $ Left e)

  {-# INLINEABLE parseJSON #-}

-- | And error code returned with 'Error' response.
newtype ErrorCode = ErrorCode { unErrorCode :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, Hashable)


-- | Response with an error with optional data.
data Error e = Error
  { errorCode    :: !ErrorCode
  , errorMessage :: !Text
  , errorData    :: !(Maybe e)
  } deriving stock (Show, Generic)

instance (ToJSON a) => ToJSON (Error a) where
  toJSON (Error !code !msg !dat) = object
          ( ("code" .= code)
          : ("message" .= msg)
          : maybe [] (\(!v) -> ["data" .= v] ) dat)

  toEncoding (Error !code !msg !dat) =
    pairs $ "code" .= code
        <> "message" .= msg
        <> maybe mempty ("data" .=) dat
  {-# INLINABLE toEncoding #-}
  {-# INLINABLE toJSON #-}

instance FromJSON a => FromJSON (Error a) where
  parseJSON = withObject "json error" $ \o ->
    Error <$> o .: "code"
          <*> o .: "message"
          <*> o .:? "data"

  {-# INLINEABLE parseJSON #-}
