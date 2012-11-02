module Graphics.UI.Awesomium.WebCore
    ( Config (..)
    , LogLevel (..)
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
    { enablePlugins            :: Bool
    , enableJavascript         :: Bool
    , enableDatabases          :: Bool
    , packagePath              :: String
    , localePath               :: String
    , userDataPath             :: String
    , pluginPath               :: String
    , logPath                  :: String
    , logLevel                 :: LogLevel
    , forceSingleProcess       :: Bool
    , childProcessPath         :: String
    , enableAutoDetectEncoding :: Bool
    , acceptLanguageOverride   :: String
    , defaultCharsetOverride   :: String
    , userAgentOverride        :: String
    , proxyServer              :: String
    , proxyConfigScript        :: String
    , authServerWhitelist      :: String
    , saveCacheAndCookies      :: Bool
    , maxCacheSize             :: Int
    , disableSameOriginPolicy  :: Bool
    , disableWinMessagePump    :: Bool
    , customCss                :: String
} deriving (Show, Read, Eq)


defaultConfig :: Config
defaultConfig = Config
    { enablePlugins            = False
    , enableJavascript         = True
    , enableDatabases          = False
    , packagePath              = ""
    , localePath               = ""
    , userDataPath             = ""
    , pluginPath               = ""
    , logPath                  = ""
    , logLevel                 = Normal
    , forceSingleProcess       = False
    , childProcessPath         = ""
    , enableAutoDetectEncoding = True
    , acceptLanguageOverride   = ""
    , defaultCharsetOverride   = ""
    , userAgentOverride        = ""
    , proxyServer              = ""
    , proxyConfigScript        = ""
    , authServerWhitelist      = ""
    , saveCacheAndCookies      = True
    , maxCacheSize             = 0
    , disableSameOriginPolicy  = False
    , disableWinMessagePump    = False
    , customCss                = "" }

-- Instantiates the WebCore singleton with a set of configuration
-- parameters.
initialize :: Config -> IO ()
initialize c@(Config a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
                     a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23)
    | c == defaultConfig = awe_webcore_initialize_default
    | otherwise = awe_webcore_initialize a1 a2 a3 a4 a5 a6 a7 a8 a9
        a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23

-- | Destroys the WebCore singleton and destroys any remaining
-- WebViews.
shutdown :: IO ()
shutdown = awe_webcore_shutdown

-- | Sets the base directory.
setBaseDirectory
    :: String -- ^ The absolute path to your base directory.  The base
              -- directory is a location that holds all of your local
              -- assets. It will be used for 'WebView.loadFile' and
              -- 'WebView.loadHTML' (to resolve relative URLs).
    -> IO ()
setBaseDirectory = awe_webcore_set_base_directory

-- | Creates a new WebView.
createWebview
    :: Int        -- ^ The width of the WebView in pixels.
    -> Int        -- ^ The height of the WebView in pixels.
    -> Bool       -- ^ Enable this to view the HTML source of any 
    -- loaded into this WebView. Default is false.
    -> IO WebView -- ^ Returns a pointer to the created WebView
    -- instance. To call method on the WebView, see loadURL and
    -- related functions.
createWebview = awe_webcore_create_webview

-- | Sets a custom response page to use when a WebView encounters
-- a certain HTML status code from the server (like '404 - File not
-- found').
setCustomResponsePage
    :: Int    -- ^ The status code this response page should be
    -- associated with. See
    -- <http://en.wikipedia.org/wiki/List_of_HTTP_status_codes>
    -> String -- ^ The local page to load as a response, a path
    -- relative to the base directory.
    -> IO ()
setCustomResponsePage = awe_webcore_set_custom_response_page

-- | Updates the WebCore and allows it to conduct various operations
-- such as updating the render buffer of each WebView, destroying any
-- WebViews that are queued for destruction, and invoking any queued
-- callback events.
update :: IO ()
update = awe_webcore_update

-- | Retrieves the base directory.
getBaseDirectory
    :: IO String -- ^ Returns a string instance representing the
    -- current base directory.
getBaseDirectory = awe_webcore_get_base_directory

-- | Returns whether or not plugins are enabled.
arePluginsEnabled :: IO Bool
arePluginsEnabled = awe_webcore_are_plugins_enabled

-- | Clear the disk cache and media cache.
clearCache :: IO ()
clearCache = awe_webcore_clear_cache

-- | Clear all cookies.
clearCookies :: IO ()
clearCookies = awe_webcore_clear_cookies

-- | Sets a cookie for a certain URL.
setCookie
    :: String -- ^ The URL to set the cookie on.
    -> String -- ^ The cookie string, for example: 
    -- /"key1=value1; key2=value2"/
    -> Bool   -- ^ Whether or not this cookie is HTTP-only.
    -> Bool   -- ^ Whether or not to force this as a session cookie.
    -> IO ()
setCookie = awe_webcore_set_cookie

-- | Gets all cookies for a certain URL.
getCookies
    :: String    -- ^ The URL whose cookies will be retrieved.
    -> Bool      -- ^ Whether or not to exclude HTTP-only cookies from
    -- the result.
    -> IO String -- ^ Returns the cookie string.
getCookies = awe_webcore_get_cookies

-- | Deletes a certain cookie on a certain URL.
deleteCookie
    :: String -- ^ The URL that we will be deleting cookies on.
    -> String -- ^ The name of the cookie that will be deleted.
    -> IO ()
deleteCookie = awe_webcore_delete_cookie

-- | Set whether or not the printer dialog should be suppressed or
-- not.  Set this to /True/ to hide printer dialogs and print
-- immediately using the OS's default printer when 'WebView.print' is
-- called.  Default is /False/ if you never call this.
setSuppressPrinterDialog
    :: Bool -- ^ Whether or not the printer dialog should be
    -- suppressed.
    -> IO ()
setSuppressPrinterDialog = awe_webcore_set_suppress_printer_dialog

-- | Query the on-disk history database.
--
--      [@note@] You must enable /saveCacheAndCookies/ (see 'Config')
--  for this method to work (otherwise no results will be returned).
queryHistory
    :: String -- ^ All results returned should match the
    -- specified text (either in the page title or
    -- in the actual text of the page itself).
    -- Specify an empty string to match anything.
    -> Int -- ^ Limit results to a specified number of days ago.
    -> Int  -- ^ Limit results to a maximum count. Specify 0 to use no
    -- limit.
    -> IO HistoryQueryResult -- ^ Returns an instance of
    -- historyQueryResult containing the results of the query. You
    -- must call awe_history_query_result_destroy once you are
    -- finished using the instance.
queryHistory = awe_webcore_query_history

