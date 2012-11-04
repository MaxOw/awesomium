----------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Awesomium.Javascript.JSObject
-- Copyright   :  (c) 2012 Maksymilian Owsianny
-- License     :  LGPL-3 (see the file LICENSE)
-- 
-- Maintainer  :  Maksymilian.Owsianny+Awesomium@gmail.com
-- Stability   :  Experimental
-- Portability :  Portable? (needs FFI)
--
----------------------------------------------------------------------

module Graphics.UI.Awesomium.Javascript.JSObject
    ( JSObject
    , create
    , destroy
    , with
    , hasProperty
    , getProperty
    , setProperty
    , getSize
    , getKeys
) where

import Graphics.UI.Awesomium.Raw

import Control.Exception (bracket)

create :: IO JSObject
create = awe_jsobject_create

destroy :: JSObject -> IO ()
destroy = awe_jsobject_destroy

with :: (JSObject -> IO b) -> IO b
with = bracket create destroy

hasProperty :: JSObject -> String -> IO Bool
hasProperty = awe_jsobject_has_property

getProperty :: JSObject -> String -> IO JSValue
getProperty = awe_jsobject_get_property

setProperty :: JSObject -> String -> JSValue -> IO ()
setProperty = awe_jsobject_set_property

getSize :: JSObject -> IO Int
getSize = awe_jsobject_get_size

getKeys :: JSObject -> IO JSArray
getKeys = awe_jsobject_get_keys

