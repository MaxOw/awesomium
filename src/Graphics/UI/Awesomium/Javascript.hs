module Graphics.UI.Awesomium.Javascript
    ( JSValue, JSValueType(..), JSArray, JSObject
    , jsvalueToJSON
    , jsarrayToJSON , jsarrayToJSONValues
    , jsobjectToJSON
) where

import Graphics.UI.Awesomium.Raw

import qualified Graphics.UI.Awesomium.Javascript.JSValue as JSValue
import qualified Graphics.UI.Awesomium.Javascript.JSArray as JSArray
import qualified Graphics.UI.Awesomium.Javascript.JSObject as JSObject

import Control.Monad (mapM, (<=<))
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Data.Yaml

jsvalueToJSON :: JSValue -> IO Value
jsvalueToJSON v = JSValue.getType v >>= \t -> case t of
    JSValueTypeNull    -> return Null
    JSValueTypeBoolean -> JSValue.toBoolean v >>= return . toJSON
    JSValueTypeInteger -> JSValue.toInteger v >>= return . toJSON
    JSValueTypeDouble  -> JSValue.toDouble  v >>= return . toJSON
    JSValueTypeString  -> JSValue.toString  v >>= return . toJSON
    JSValueTypeObject  -> JSValue.getObject v >>= jsobjectToJSON
    JSValueTypeArray   -> JSValue.getArray  v >>= jsarrayToJSON

jsarrayToJSON :: JSArray -> IO Value
jsarrayToJSON = return . array <=< jsarrayToJSONValues

jsarrayToJSONValues :: JSArray -> IO [Value]
jsarrayToJSONValues a = JSArray.getSize a >>= \s ->
    mapM (jsvalueToJSON <=< JSArray.getElement a) [0..s-1]

jsobjectToJSON :: JSObject -> IO Value
jsobjectToJSON o = do
    ks <- getStrKeys =<< JSObject.getKeys o
    vs <- mapM (jsvalueToJSON <=< JSObject.getProperty o) ks
    return . object $ zipWith (.=) (map T.pack ks) vs
    where
        getStrKeys :: JSArray -> IO [String]
        getStrKeys a = JSArray.getSize a >>= \s ->
            mapM (takeStr <=< JSArray.getElement a) [0..s-1]
            >>= return . catMaybes
        
        takeStr :: JSValue -> IO (Maybe String)
        takeStr v = JSValue.getType v >>= \t ->
            if (t == JSValueTypeString)
                then JSValue.toString v >>= return . Just
                else return Nothing

{-
instance ToJSON JSValue where
    toJSON x = unsafePreformIO $ jsvalueToJSON x

instance ToJSON JSArray where
    toJSON x = unsafePreformIO $ jsarrayToJSON x

instance ToJSON JSObject where
    toJSON x = unsafePreformIO $ jsobjectToJSON x
-}
