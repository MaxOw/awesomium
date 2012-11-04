----------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Awesomium.UploadElement
-- Copyright   :  (c) 2012 Maksymilian Owsianny
-- License     :  LGPL-3 (see the file LICENSE)
-- 
-- Maintainer  :  Maksymilian.Owsianny+Awesomium@gmail.com
-- Stability   :  Experimental
-- Portability :  Portable? (needs FFI)
--
----------------------------------------------------------------------

module Graphics.UI.Awesomium.UploadElement
    ( UploadElement
    , isFilePath
    , isBytes
    , getBytes
    , getFilePath
) where

import Graphics.UI.Awesomium.Raw

-- | Whether or not this 'UploadElement' is a file.
isFilePath :: UploadElement -> IO Bool
isFilePath = awe_upload_element_is_file_path

-- | Whether or not this 'UploadElement' is a string of bytes.
isBytes :: UploadElement -> IO Bool
isBytes = awe_upload_element_is_bytes

-- | Get the string of bytes associated with this 'UploadElement'.
getBytes :: UploadElement -> IO String
getBytes = awe_upload_element_get_bytes

-- | Get the file path associated with this 'UploadElement'.
getFilePath :: UploadElement -> IO String
getFilePath = awe_upload_element_get_file_path

