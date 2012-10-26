module Graphics.UI.Awesomium.Javascript.JSArray
    ( JSArray
    , create
    , destroy
    , getSize
    , getElement
) where

import Graphics.UI.Awesomium.Raw

import Foreign.Ptr (Ptr)

create :: Ptr JSValue -> Int -> IO JSArray
create = awe_jsarray_create

destroy :: JSArray -> IO ()
destroy = awe_jsarray_destroy

getSize :: JSArray -> IO Int
getSize = awe_jsarray_get_size

getElement :: JSArray -> Int -> IO JSValue
getElement = awe_jsarray_get_element

