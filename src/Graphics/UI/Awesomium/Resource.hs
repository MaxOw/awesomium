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

responseCreateFromFile :: String -> IO ResourceResponse
responseCreateFromFile = awe_resource_response_create_from_file

requestCancel :: ResourceRequest -> IO ()
requestCancel = awe_resource_request_cancel

requestGetUrl :: ResourceRequest -> IO String
requestGetUrl = awe_resource_request_get_url

requestGetMethod :: ResourceRequest -> IO String
requestGetMethod = awe_resource_request_get_method

requestSetMethod :: ResourceRequest -> String -> IO ()
requestSetMethod = awe_resource_request_set_method

requestGetReferrer :: ResourceRequest -> IO String
requestGetReferrer = awe_resource_request_get_referrer

requestSetReferrer :: ResourceRequest -> String -> IO ()
requestSetReferrer = awe_resource_request_set_referrer

requestGetExtraHeaders :: ResourceRequest -> IO String
requestGetExtraHeaders = awe_resource_request_get_extra_headers

requestSetExtraHeaders :: ResourceRequest -> String -> IO ()
requestSetExtraHeaders = awe_resource_request_set_extra_headers

requestAppendExtraHeader :: ResourceRequest -> String -> String -> IO ()
requestAppendExtraHeader = awe_resource_request_append_extra_header

requestGetNumUploadElements :: ResourceRequest -> IO Int
requestGetNumUploadElements = awe_resource_request_get_num_upload_elements

requestGetUploadElement :: ResourceRequest -> Int -> IO UploadElement
requestGetUploadElement = awe_resource_request_get_upload_element

requestClearUploadElements :: ResourceRequest -> IO ()
requestClearUploadElements = awe_resource_request_clear_upload_elements

requestAppendUploadFilePath :: ResourceRequest -> String -> IO ()
requestAppendUploadFilePath = awe_resource_request_append_upload_file_path

requestAppendUploadBytes :: ResourceRequest -> String -> IO ()
requestAppendUploadBytes = awe_resource_request_append_upload_bytes

