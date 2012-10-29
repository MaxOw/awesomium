module Graphics.UI.Awesomium.Javascript.JSArray
    ( JSArray
    , create
    , destroy
    , with
    , getSize
    , getElement
) where

import Graphics.UI.Awesomium.Raw

import Control.Exception (bracket)
import Foreign (withArray)
import Foreign.Ptr (Ptr)

create :: Ptr JSValue -> Int -> IO JSArray
create = awe_jsarray_create

destroy :: JSArray -> IO ()
destroy = awe_jsarray_destroy

with :: [JSValue] -> (JSArray -> IO b) -> IO b
with vs f = withArray vs $ \ptr ->
            bracket (create ptr (length vs)) destroy f

getSize :: JSArray -> IO Int
getSize = awe_jsarray_get_size

getElement :: JSArray -> Int -> IO JSValue
getElement = awe_jsarray_get_element

