module Graphics.UI.Awesomium.WebCore
    ( Config
    , defaultConfig
    , initialize
    , shutdown
    , setBaseDirectory
    , createWebview
    , setCustomResponsePage
    , update
    , getBaseDirectory
    , arePluginsEnabled
    , clearCache
    , clearCookies
    , setCookie
    , getCookies
    , deleteCookie
    , setSuppressPrinterDialog
    , queryHistory
) where

import Graphics.UI.Awesomium.Raw

data Config = Config 
    { pluginsEnabled            :: Bool
    , javascriptEnabled         :: Bool
    , databasesEnabled          :: Bool
    , packagePath               :: String
    , localePath                :: String
    , userDataPath              :: String
    , pluginPath                :: String
    , logPath                   :: String
    , logLevel                  :: LogLevel
    , singleProcess             :: Bool
    , childProcessPath          :: String
    , autoDetectEncodingEnabled :: Bool
    , acceptLanguageOverride    :: String
    , defaultCharsetOrerride    :: String
    , userAgentOverride         :: String
    , proxyServer               :: String
    , proxyConfigScript         :: String
    , authServerWhitelist       :: String
    , saveCacheAndCookies       :: Bool
    , maxCacheSize              :: Int
    , sameOriginPolicyEnabled   :: Bool
    , winMessagePumpEnabled     :: Bool
    , customCss                 :: String
} deriving (Show, Read, Eq)


defaultConfig :: Config
defaultConfig = Config
    { pluginsEnabled            = False
    , javascriptEnabled         = True
    , databasesEnabled          = False
    , packagePath               = ""
    , localePath                = ""
    , userDataPath              = ""
    , pluginPath                = ""
    , logPath                   = ""
    , logLevel                  = Normal
    , singleProcess             = False
    , childProcessPath          = ""
    , autoDetectEncodingEnabled = True
    , acceptLanguageOverride    = ""
    , defaultCharsetOrerride    = ""
    , userAgentOverride         = ""
    , proxyServer               = ""
    , proxyConfigScript         = ""
    , authServerWhitelist       = ""
    , saveCacheAndCookies       = True
    , maxCacheSize              = 0
    , sameOriginPolicyEnabled   = True
    , winMessagePumpEnabled     = True
    , customCss                 = "" }

initialize :: Config -> IO ()
initialize c@(Config a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
                     a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23)
    | c == defaultConfig = awe_webcore_initialize_default
    | otherwise = awe_webcore_initialize a1 a2 a3 a4 a5 a6 a7 a8 a9
        a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23

shutdown :: IO ()
shutdown = awe_webcore_shutdown

setBaseDirectory :: String -> IO ()
setBaseDirectory = awe_webcore_set_base_directory

createWebview :: Int -> Int -> Bool -> IO WebView
createWebview = awe_webcore_create_webview

setCustomResponsePage :: Int -> String -> IO ()
setCustomResponsePage = awe_webcore_set_custom_response_page

update :: IO ()
update = awe_webcore_update

getBaseDirectory :: IO String
getBaseDirectory = awe_webcore_get_base_directory

arePluginsEnabled :: IO Bool
arePluginsEnabled = awe_webcore_are_plugins_enabled

clearCache :: IO ()
clearCache = awe_webcore_clear_cache

clearCookies :: IO ()
clearCookies = awe_webcore_clear_cookies

setCookie :: String -> String -> Bool -> Bool -> IO ()
setCookie = awe_webcore_set_cookie

getCookies :: String -> Bool -> IO String
getCookies = awe_webcore_get_cookies

deleteCookie :: String -> String -> IO ()
deleteCookie = awe_webcore_delete_cookie

setSuppressPrinterDialog :: Bool -> IO ()
setSuppressPrinterDialog = awe_webcore_set_suppress_printer_dialog

queryHistory :: String -> Int -> Int -> IO HistoryQueryResult
queryHistory = awe_webcore_query_history

