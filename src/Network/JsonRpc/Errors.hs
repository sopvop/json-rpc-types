{-# LANGUAGE OverloadedStrings #-}
-- | Module to help construct errors which may occur in json-rpc calls
module Network.JsonRpc.Errors
    ( -- * Errors
      parseError
    , invalidRequest
    , methodNotFound
    , invalidParams
    , internalError
    , serverError
    , parseErrorCode
    , invalidRequestCode
    , invalidParamsCode
    , internalErrorCode
      -- * Re-exports from "SmAssetMan.Network.JsonRpc.Types"
    , ErrorCode (..)
    , Error
    , RequestId
    ) where


import           Network.JsonRpc.Types
    (Error (..), ErrorCode (..), RequestId (..), Response (..))


parseErrorCode :: ErrorCode
parseErrorCode = ErrorCode (-32700)

invalidRequestCode :: ErrorCode
invalidRequestCode = ErrorCode (-32600)

methodNotFoundCode :: ErrorCode
methodNotFoundCode = ErrorCode (-32601)

invalidParamsCode :: ErrorCode
invalidParamsCode = ErrorCode (-32602)

internalErrorCode :: ErrorCode
internalErrorCode = ErrorCode (-32603)

-- | Error while parsing json request. Only useful for syncronous calls.
parseError :: Maybe e -> Response e a
parseError = Response (RequestIdInt (-1)) . Just . Left
             . Error parseErrorCode "Parse error"

-- | The JSON sent is not a valid Request object.
invalidRequest :: RequestId -> Maybe e -> Response e a
invalidRequest rid = Response rid . Just . Left
                     . Error invalidRequestCode "Invalid Request"

-- | The method does not exist / is not available.
methodNotFound :: RequestId -> Maybe e -> Response e a
methodNotFound rid = Response rid . Just . Left
                     . Error methodNotFoundCode "Method not found"

-- | Invalid method parameter(s).
invalidParams :: RequestId -> Maybe e -> Response e a
invalidParams rid = Response rid . Just . Left
                    . Error invalidParamsCode "Invalid params"

-- | Internal JSON-RPC error.
internalError :: RequestId -> Maybe e -> Response e a
internalError rid = Response rid . Just . Left
                    . Error internalErrorCode "Internal error"

-- | Reserved for implementation-defined server-errors.
-- Error code should be in -32000 to -32099 range
serverError :: ErrorCode -> RequestId -> Maybe e -> Response e a
serverError code rid = Response rid . Just . Left . Error code "Server Error"
