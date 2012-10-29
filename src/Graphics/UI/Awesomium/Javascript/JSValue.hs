module Graphics.UI.Awesomium.Javascript.JSValue
    ( JSValue, JSValueType(..)
    , createNullValue
    , createBoolValue
    , createIntegerValue
    , createDoubleValue
    , createStringValue
    , createObjectValue
    , createArrayValue
    , destroy
    , with
    , getType
    , toString
    , toInteger
    , toDouble
    , toBoolean
    , getArray
    , getObject
) where

import Graphics.UI.Awesomium.Raw

import Prelude (IO, Int, Double, Bool, String)
import Control.Exception (bracket)

createNullValue :: IO JSValue
createNullValue = awe_jsvalue_create_null_value

createBoolValue :: Bool -> IO JSValue
createBoolValue = awe_jsvalue_create_bool_value

createIntegerValue :: Int -> IO JSValue
createIntegerValue = awe_jsvalue_create_integer_value

createDoubleValue :: Double -> IO JSValue
createDoubleValue = awe_jsvalue_create_double_value

createStringValue :: String -> IO JSValue
createStringValue = awe_jsvalue_create_string_value

createObjectValue :: JSObject -> IO JSValue
createObjectValue = awe_jsvalue_create_object_value

createArrayValue :: JSArray -> IO JSValue
createArrayValue = awe_jsvalue_create_array_value

destroy :: JSValue -> IO ()
destroy = awe_jsvalue_destroy

with :: IO JSValue -> (JSValue -> IO b) -> IO b
with c = bracket c destroy

getType :: JSValue -> IO JSValueType
getType = awe_jsvalue_get_type

toString :: JSValue -> IO String
toString = awe_jsvalue_to_string

toInteger :: JSValue -> IO Int
toInteger = awe_jsvalue_to_integer

toDouble :: JSValue -> IO Double
toDouble = awe_jsvalue_to_double

toBoolean :: JSValue -> IO Bool
toBoolean = awe_jsvalue_to_boolean

getArray :: JSValue -> IO JSArray
getArray = awe_jsvalue_get_array

getObject :: JSValue -> IO JSObject
getObject = awe_jsvalue_get_object

