module Graphics.UI.Awesomium.Javascript.JSObject
    ( JSObject
    , create
    , destroy
    , hasProperty
    , getProperty
    , setProperty
    , getSize
    , getKeys
) where

import Graphics.UI.Awesomium.Raw

create :: IO JSObject
create = awe_jsobject_create

destroy :: JSObject -> IO ()
destroy = awe_jsobject_destroy

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

