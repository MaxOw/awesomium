-- | You shouldn't ever have to use functions from this module or its
-- sub-modules as all functions that use Javascript already convert
-- everything to more friendly JSON 'Data.Aeson.Value' from
-- "Data.Aeson" module.
module Graphics.UI.Awesomium.Javascript
    ( JSValue, JSValueType(..), JSArray, JSObject
    , jsvalueToJSON
    , jsarrayToJSON, jsarrayToJSONValues
    , jsobjectToJSON
    , withJS, withJSArray
) where

import Graphics.UI.Awesomium.Raw

import qualified Graphics.UI.Awesomium.Javascript.JSValue as JSValue
import qualified Graphics.UI.Awesomium.Javascript.JSArray as JSArray
import qualified Graphics.UI.Awesomium.Javascript.JSObject as JSObject

import Foreign.Marshal (withArray, withMany)
import Control.Monad (mapM, (<=<))
import Control.Exception (bracket)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Aeson.Types
import Data.Attoparsec.Number

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
jsarrayToJSON = return . Array . V.fromList <=< jsarrayToJSONValues

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

withJS :: Value -> (JSValue -> IO ()) -> IO ()
withJS v f = case v of
    Null           -> brt JSValue.createNullValue
    (Bool _)       -> fv $ brt . JSValue.createBoolValue
    (Number (I _)) -> fv $ brt . JSValue.createIntegerValue
    (Number (D _)) -> fv $ brt . JSValue.createDoubleValue
    (String _)     -> fv $ brt . JSValue.createStringValue
    (Object _)     -> fv $ \ps ->
        JSObject.with $ \ob ->
            let (ns, vs) = (unzip . M.toList) ps in
            withMany withJS vs $ \vs' ->
                mapM_ (uncurry $ JSObject.setProperty ob) 
                      (zip (map T.unpack ns) vs')
                >> (brt $ JSValue.createObjectValue ob)
    (Array _)      -> fv $ \vs ->
        withMany withJS vs $ \vs' ->
            JSArray.with vs' $ \ar ->
                brt $ JSValue.createArrayValue ar
    where brt c = JSValue.with c f
          fv r = either putStrLn r $ parseEither parseJSON v

withJSArray :: [Value] -> (JSArray -> IO ()) -> IO ()
withJSArray vs f = withMany withJS vs $ flip JSArray.with f

{-
instance ToJSON JSValue where
    toJSON x = unsafePreformIO $ jsvalueToJSON x

instance ToJSON JSArray where
    toJSON x = unsafePreformIO $ jsarrayToJSON x

instance ToJSON JSObject where
    toJSON x = unsafePreformIO $ jsobjectToJSON x
-}
