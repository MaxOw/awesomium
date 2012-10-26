module Graphics.UI.Awesomium.UploadElement
    ( UploadElement
    , isFilePath
    , isBytes
    , getBytes
    , getFilePath
) where

import Graphics.UI.Awesomium.Raw

isFilePath :: UploadElement -> IO Bool
isFilePath = awe_upload_element_is_file_path

isBytes :: UploadElement -> IO Bool
isBytes = awe_upload_element_is_bytes

getBytes :: UploadElement -> IO String
getBytes = awe_upload_element_get_bytes

getFilePath :: UploadElement -> IO String
getFilePath = awe_upload_element_get_file_path

