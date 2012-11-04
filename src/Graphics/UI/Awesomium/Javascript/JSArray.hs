----------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Awesomium.Javascript.JSArray
-- Copyright   :  (c) 2012 Maksymilian Owsianny
-- License     :  LGPL-3 (see the file LICENSE)
-- 
-- Maintainer  :  Maksymilian.Owsianny+Awesomium@gmail.com
-- Stability   :  Experimental
-- Portability :  Portable? (needs FFI)
--
----------------------------------------------------------------------

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

