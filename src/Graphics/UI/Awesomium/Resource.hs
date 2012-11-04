----------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Awesomium.Resource
-- Copyright   :  (c) 2012 Maksymilian Owsianny
-- License     :  LGPL-3 (see the file LICENSE)
-- 
-- Maintainer  :  Maksymilian.Owsianny+Awesomium@gmail.com
-- Stability   :  Experimental
-- Portability :  Portable? (needs FFI)
--
----------------------------------------------------------------------

module Graphics.UI.Awesomium.Resource
    ( ResourceResponse, ResourceRequest
 -- , responseCreate
    , responseCreateFromFile
    , requestCancel
    , requestGetUrl
    , requestGetMethod
    , requestSetMethod
    , requestGetReferrer
    , requestSetReferrer
    , requestGetExtraHeaders
    , requestSetExtraHeaders
    , requestAppendExtraHeader
    , requestGetNumUploadElements
    , requestGetUploadElement
    , requestClearUploadElements
    , requestAppendUploadFilePath
    , requestAppendUploadBytes
) where

import Graphics.UI.Awesomium.Raw

-- {#fun awe_webview_set_callback_resource_request :: WebView', resource_response* (*callback } -> IO ()
-- {#fun awe_webview_set_callback_resource_response :: WebView', void (*callback } -> IO ()

-- responseCreate :: size_t num_bytes, unsigned char* buffer, `String -> IO ResourceResponse
-- responseCreate = awe_resource_response_create

-- | Create a ResourceResponse from a file on disk.
responseCreateFromFile :: String -> IO ResourceResponse
responseCreateFromFile = awe_resource_response_create_from_file

-- | Cancel the request (this is useful for blocking a resource load).
requestCancel :: ResourceRequest -> IO ()
requestCancel = awe_resource_request_cancel

-- | Get the URL associated with this request.
requestGetUrl :: ResourceRequest -> IO String
requestGetUrl = awe_resource_request_get_url

-- | Get the HTTP method (usually @GET@ or @POST@).
requestGetMethod :: ResourceRequest -> IO String
requestGetMethod = awe_resource_request_get_method

-- | Set the HTTP method
requestSetMethod :: ResourceRequest -> String -> IO ()
requestSetMethod = awe_resource_request_set_method

-- | Get the referrer.
requestGetReferrer :: ResourceRequest -> IO String
requestGetReferrer = awe_resource_request_get_referrer

-- | Set the referrer.
requestSetReferrer :: ResourceRequest -> String -> IO ()
requestSetReferrer = awe_resource_request_set_referrer

-- | Get extra headers for the request.
requestGetExtraHeaders :: ResourceRequest -> IO String
requestGetExtraHeaders = awe_resource_request_get_extra_headers

-- | Override extra headers for the request, delimited by \/r\/n (CRLF).
--
-- Format should be:
--   Name: Value\/r\/nName: Value\/r\/nName: Value
--
-- Headers should NOT end in \/r\/n (CRLF)
requestSetExtraHeaders :: ResourceRequest -> String -> IO ()
requestSetExtraHeaders = awe_resource_request_set_extra_headers

-- | Append an extra header to the request.
requestAppendExtraHeader
    :: ResourceRequest
    -> String -- ^ Name of the header
    -> String -- ^ Value of the header
    -> IO ()
requestAppendExtraHeader = awe_resource_request_append_extra_header

-- | Get the number of upload elements (essentially, batches of POST
-- data).
requestGetNumUploadElements :: ResourceRequest -> IO Int
requestGetNumUploadElements =
    awe_resource_request_get_num_upload_elements

-- | Get a certain upload element (returned instance is owned by this
-- class)
requestGetUploadElement :: ResourceRequest -> Int -> IO UploadElement
requestGetUploadElement = awe_resource_request_get_upload_element

-- | Clear all upload elements
requestClearUploadElements :: ResourceRequest -> IO ()
requestClearUploadElements =
    awe_resource_request_clear_upload_elements

-- | Append a file for POST data (adds a new 'UploadElement')
requestAppendUploadFilePath :: ResourceRequest -> String -> IO ()
requestAppendUploadFilePath =
    awe_resource_request_append_upload_file_path

-- | Append a string of bytes for POST data (adds a new
-- 'UploadElement')
requestAppendUploadBytes :: ResourceRequest -> String -> IO ()
requestAppendUploadBytes = awe_resource_request_append_upload_bytes

