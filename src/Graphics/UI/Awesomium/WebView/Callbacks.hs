----------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Awesomium.WebView.Callbacks
-- Copyright   :  (c) 2012 Maksymilian Owsianny
-- License     :  LGPL-3 (see the file LICENSE)
-- 
-- Maintainer  :  Maksymilian.Owsianny+Awesomium@gmail.com
-- Stability   :  Experimental
-- Portability :  Portable? (needs FFI)
--
----------------------------------------------------------------------

module Graphics.UI.Awesomium.WebView.Callbacks
    ( BeginNavigationCallbackHandler      , setCallbackBeginNavigation
    , BeginLoadingCallbackHandler         , setCallbackBeginLoading
    , FinishLoadingCallbackHandler        , setCallbackFinishLoading
    , JSCallbackHandler                   , setCallbackJS
    , ReceiveTitleCallbackHandler         , setCallbackReceiveTitle
    , ChangeTooltipCallbackHandler        , setCallbackChangeTooltip
    , CursorType (..)
    , ChangeCursorCallbackHandler         , setCallbackChangeCursor
    , ChangeKeyboardFocusCallbackHandler  , setCallbackChangeKeyboardFocus
    , ChangeTargetUrlCallbackHandler      , setCallbackChangeTargetUrl
    , OpenExternalLinkCallbackHandler     , setCallbackOpenExternalLink
    , RequestDownloadCallbackHandler      , setCallbackRequestDownload
    , WebViewCrashedCallbackHandler       , setCallbackWebViewCrashed
    , PluginCrashedCallbackHandler        , setCallbackPluginCrashed
    , RequestMoveCallbackHandler          , setCallbackRequestMove
    , GetPageContentsCallbackHandler      , setCallbackGetPageContents
    , DomReadyCallbackHandler             , setCallbackDomReady
    , RequestFileChooserCallbackHandler   , setCallbackRequestFileChooser
    , GetScrollDataCallbackHandler        , setCallbackGetScrollData
    , JsConsoleMessageCallbackHandler     , setCallbackJsConsoleMessage
    , GetFindResultsCallbackHandler       , setCallbackGetFindResults
    , ImeState (..)
    , UpdateImeCallbackHandler            , setCallbackUpdateIme
    , MediaType (..)
    , ShowContextMenuCallbackHandler      , setCallbackShowContextMenu
    , RequestLoginCallbackHandler         , setCallbackRequestLogin
    , ChangeHistoryCallbackHandler        , setCallbackChangeHistory
    , FinishResizeCallbackHandler         , setCallbackFinishResize
    , ShowJavascriptDialogCallbackHandler , setCallbackShowJavascriptDialog
) where

import Graphics.UI.Awesomium.Raw
import Graphics.UI.Awesomium.Javascript

import Foreign (peek)
import Foreign.Ptr (FunPtr)
import Foreign.Marshal.Utils (toBool)
import Data.Aeson

(>>=>) :: Monad m => m a -> (a -> m b) -> m a
(>>=>) a f = a >>= \r -> f r >> return r

type BeginNavigationCallbackHandler = WebView -> String -> String -> IO()
defBeginNavigationCallback :: BeginNavigationCallbackHandler -> BeginNavigationCallback
defBeginNavigationCallback convcb wv a1 a0 = do
    ar1 <- fromAweString a1
    ar0 <- fromAweString a0
    convcb wv ar1 ar0

-- | Assign a callback function to be notified when a WebView begins
-- navigation to a certain URL.
setCallbackBeginNavigation :: WebView -> BeginNavigationCallbackHandler -> IO (FunPtr BeginNavigationCallback)
setCallbackBeginNavigation wv ah = 
    mkBeginNavigationCallback (defBeginNavigationCallback ah) >>=>
    awe_webview_set_callback_begin_navigation wv

type BeginLoadingCallbackHandler = WebView -> String -> String -> Int -> String -> IO()
defBeginLoadingCallback :: BeginLoadingCallbackHandler -> BeginLoadingCallback
defBeginLoadingCallback convcb wv a1 a2 a3 a0 = do
    ar1 <- fromAweString a1
    ar2 <- fromAweString a2
    let ar3 = fromIntegral a3
    ar0 <- fromAweString a0
    convcb wv ar1 ar2 ar3 ar0

-- | Assign a callback function to be notified when a WebView begins
-- to actually receive data from a server.
setCallbackBeginLoading :: WebView -> BeginLoadingCallbackHandler -> IO (FunPtr BeginLoadingCallback)
setCallbackBeginLoading wv ah = 
    mkBeginLoadingCallback (defBeginLoadingCallback ah) >>=>
    awe_webview_set_callback_begin_loading wv

type FinishLoadingCallbackHandler = WebView -> IO()
defFinishLoadingCallback :: FinishLoadingCallbackHandler -> FinishLoadingCallback
defFinishLoadingCallback convcb wv = do
    convcb wv

-- | Assign a callback function to be notified when a WebView has
-- finished all loads.
setCallbackFinishLoading :: WebView -> FinishLoadingCallbackHandler -> IO (FunPtr FinishLoadingCallback)
setCallbackFinishLoading wv ah = 
    mkFinishLoadingCallback (defFinishLoadingCallback ah) >>=>
    awe_webview_set_callback_finish_loading wv

type JSCallbackHandler = WebView -> String -> String -> [Value] -> IO()
defJSCallback :: JSCallbackHandler -> JSCallback
defJSCallback convcb wv a1 a2 a0 = do
    ar1 <- fromAweString a1
    ar2 <- fromAweString a2
    ar0 <- jsarrayToJSONValues a0
    convcb wv ar1 ar2 ar0

-- | Assign a callback function to be notified when a Javascript
-- object callback has been invoked on a page.
setCallbackJS :: WebView -> JSCallbackHandler -> IO (FunPtr JSCallback)
setCallbackJS wv ah = 
    mkJSCallback (defJSCallback ah) >>=>
    awe_webview_set_callback_js_callback wv

type ReceiveTitleCallbackHandler = WebView -> String -> String -> IO()
defReceiveTitleCallback :: ReceiveTitleCallbackHandler -> ReceiveTitleCallback
defReceiveTitleCallback convcb wv a1 a0 = do
    ar1 <- fromAweString a1
    ar0 <- fromAweString a0
    convcb wv ar1 ar0

-- | Assign a callback function to be notified when a page title is
-- received.
setCallbackReceiveTitle :: WebView -> ReceiveTitleCallbackHandler -> IO (FunPtr ReceiveTitleCallback)
setCallbackReceiveTitle wv ah = 
    mkReceiveTitleCallback (defReceiveTitleCallback ah) >>=>
    awe_webview_set_callback_receive_title wv

type ChangeTooltipCallbackHandler = WebView -> String -> IO()
defChangeTooltipCallback :: ChangeTooltipCallbackHandler -> ChangeTooltipCallback
defChangeTooltipCallback convcb wv a0 = do
    ar0 <- fromAweString a0
    convcb wv ar0

-- | Assign a callback function to be notified when a tooltip has
-- changed state.
setCallbackChangeTooltip :: WebView -> ChangeTooltipCallbackHandler -> IO (FunPtr ChangeTooltipCallback)
setCallbackChangeTooltip wv ah = 
    mkChangeTooltipCallback (defChangeTooltipCallback ah) >>=>
    awe_webview_set_callback_change_tooltip wv

type ChangeCursorCallbackHandler = WebView -> CursorType -> IO()
defChangeCursorCallback :: ChangeCursorCallbackHandler -> ChangeCursorCallback
defChangeCursorCallback convcb wv a0 = do
    let ar0 = toEnum . fromIntegral $ a0
    convcb wv ar0

-- | Assign a callback function to be notified when a cursor has
-- changed state.
setCallbackChangeCursor :: WebView -> ChangeCursorCallbackHandler -> IO (FunPtr ChangeCursorCallback)
setCallbackChangeCursor wv ah = 
    mkChangeCursorCallback (defChangeCursorCallback ah) >>=>
    awe_webview_set_callback_change_cursor wv

type ChangeKeyboardFocusCallbackHandler = WebView -> Bool -> IO()
defChangeKeyboardFocusCallback :: ChangeKeyboardFocusCallbackHandler -> ChangeKeyboardFocusCallback
defChangeKeyboardFocusCallback convcb wv a0 = do
    let ar0 = toBool a0
    convcb wv ar0

-- | Assign a callback function to be notified when keyboard focus has
-- changed.
setCallbackChangeKeyboardFocus :: WebView -> ChangeKeyboardFocusCallbackHandler -> IO (FunPtr ChangeKeyboardFocusCallback)
setCallbackChangeKeyboardFocus wv ah = 
    mkChangeKeyboardFocusCallback (defChangeKeyboardFocusCallback ah) >>=>
    awe_webview_set_callback_change_keyboard_focus wv

type ChangeTargetUrlCallbackHandler = WebView -> String -> IO()
defChangeTargetUrlCallback :: ChangeTargetUrlCallbackHandler -> ChangeTargetUrlCallback
defChangeTargetUrlCallback convcb wv a0 = do
    ar0 <- fromAweString a0
    convcb wv ar0

-- | Assign a callback function to be notified when the target URL has
-- changed.  This is usually the result of hovering over a link on the
-- page.
setCallbackChangeTargetUrl :: WebView -> ChangeTargetUrlCallbackHandler -> IO (FunPtr ChangeTargetUrlCallback)
setCallbackChangeTargetUrl wv ah = 
    mkChangeTargetUrlCallback (defChangeTargetUrlCallback ah) >>=>
    awe_webview_set_callback_change_target_url wv

type OpenExternalLinkCallbackHandler = WebView -> String -> String -> IO()
defOpenExternalLinkCallback :: OpenExternalLinkCallbackHandler -> OpenExternalLinkCallback
defOpenExternalLinkCallback convcb wv a1 a0 = do
    ar1 <- fromAweString a1
    ar0 <- fromAweString a0
    convcb wv ar1 ar0

-- | Assign a callback function to be notified when an external link
-- is attempted to be opened. An external link is any link that
-- normally opens in a new window in a standard browser (for example,
-- links with @target=\"_blank\"@, calls to @window.open(url)@, and
-- URL open events from Flash plugins).
setCallbackOpenExternalLink :: WebView -> OpenExternalLinkCallbackHandler -> IO (FunPtr OpenExternalLinkCallback)
setCallbackOpenExternalLink wv ah = 
    mkOpenExternalLinkCallback (defOpenExternalLinkCallback ah) >>=>
    awe_webview_set_callback_open_external_link wv

type RequestDownloadCallbackHandler = WebView -> String -> IO()
defRequestDownloadCallback :: RequestDownloadCallbackHandler -> RequestDownloadCallback
defRequestDownloadCallback convcb wv a0 = do
    ar0 <- fromAweString a0
    convcb wv ar0

-- | Assign a callback function to be notified when a page requests
-- for a certain URL to be downloaded by the user.
setCallbackRequestDownload :: WebView -> RequestDownloadCallbackHandler -> IO (FunPtr RequestDownloadCallback)
setCallbackRequestDownload wv ah = 
    mkRequestDownloadCallback (defRequestDownloadCallback ah) >>=>
    awe_webview_set_callback_request_download wv

type WebViewCrashedCallbackHandler = WebView -> IO()
defWebViewCrashedCallback :: WebViewCrashedCallbackHandler -> WebViewCrashedCallback
defWebViewCrashedCallback convcb wv = do
    convcb wv

-- | Assign a callback function to be notified when the renderer for
-- a certain WebView (which is isolated in a separate process) crashes
-- unexpectedly.
setCallbackWebViewCrashed :: WebView -> WebViewCrashedCallbackHandler -> IO (FunPtr WebViewCrashedCallback)
setCallbackWebViewCrashed wv ah = 
    mkWebViewCrashedCallback (defWebViewCrashedCallback ah) >>=>
    awe_webview_set_callback_web_view_crashed wv

type PluginCrashedCallbackHandler = WebView -> String -> IO()
defPluginCrashedCallback :: PluginCrashedCallbackHandler -> PluginCrashedCallback
defPluginCrashedCallback convcb wv a0 = do
    ar0 <- fromAweString a0
    convcb wv ar0

-- | Assign a callback function to be notified when when the renderer
-- for a certain plugin (usually Flash, which is isolated in
-- a separate process) crashes unexpectedly.
setCallbackPluginCrashed :: WebView -> PluginCrashedCallbackHandler -> IO (FunPtr PluginCrashedCallback)
setCallbackPluginCrashed wv ah = 
    mkPluginCrashedCallback (defPluginCrashedCallback ah) >>=>
    awe_webview_set_callback_plugin_crashed wv

type RequestMoveCallbackHandler = WebView -> Int -> Int -> IO()
defRequestMoveCallback :: RequestMoveCallbackHandler -> RequestMoveCallback
defRequestMoveCallback convcb wv a1 a0 = do
    let ar1 = fromIntegral a1
    let ar0 = fromIntegral a0
    convcb wv ar1 ar0

-- | Assign a callback function to be notified when the page requests
-- for the containing window to be moved to a certain location on the
-- screen.
setCallbackRequestMove :: WebView -> RequestMoveCallbackHandler -> IO (FunPtr RequestMoveCallback)
setCallbackRequestMove wv ah = 
    mkRequestMoveCallback (defRequestMoveCallback ah) >>=>
    awe_webview_set_callback_request_move wv

type GetPageContentsCallbackHandler = WebView -> String -> String -> IO()
defGetPageContentsCallback :: GetPageContentsCallbackHandler -> GetPageContentsCallback
defGetPageContentsCallback convcb wv a1 a0 = do
    ar1 <- fromAweString a1
    ar0 <- fromAweString a0
    convcb wv ar1 ar0

-- | Assign a callback function to be notified when the contents of
-- the page has finished loading. This occurs at the end of most page
-- loads.
setCallbackGetPageContents :: WebView -> GetPageContentsCallbackHandler -> IO (FunPtr GetPageContentsCallback)
setCallbackGetPageContents wv ah = 
    mkGetPageContentsCallback (defGetPageContentsCallback ah) >>=>
    awe_webview_set_callback_get_page_contents wv

type DomReadyCallbackHandler = WebView -> IO()
defDomReadyCallback :: DomReadyCallbackHandler -> DomReadyCallback
defDomReadyCallback convcb wv = do
    convcb wv

-- | Assign a callback function to be notified once the DOM (Document
-- Object Model) for a page is ready. This is very useful for
-- executing Javascript on a page before its content has finished
-- loading.
setCallbackDomReady :: WebView -> DomReadyCallbackHandler -> IO (FunPtr DomReadyCallback)
setCallbackDomReady wv ah = 
    mkDomReadyCallback (defDomReadyCallback ah) >>=>
    awe_webview_set_callback_dom_ready wv

type RequestFileChooserCallbackHandler = WebView -> Bool -> String -> String -> IO()
defRequestFileChooserCallback :: RequestFileChooserCallbackHandler -> RequestFileChooserCallback
defRequestFileChooserCallback convcb wv a1 a2 a0 = do
    let ar1 = toBool a1
    ar2 <- fromAweString a2
    ar0 <- fromAweString a0
    convcb wv ar1 ar2 ar0

-- | Assign a callback function to be notified whenever a page
-- requests a file chooser dialog to be displayed (usually the result
-- of an @input@ element with type @file@ being clicked by a user).
-- You will need to display your own dialog (it does not have to be
-- modal, this request does not block).  Once a file has been chosen
-- by the user, 'Graphics.UI.Awesomium.WebView.chooseFile'
-- should be called.
setCallbackRequestFileChooser :: WebView -> RequestFileChooserCallbackHandler -> IO (FunPtr RequestFileChooserCallback)
setCallbackRequestFileChooser wv ah = 
    mkRequestFileChooserCallback (defRequestFileChooserCallback ah) >>=>
    awe_webview_set_callback_request_file_chooser wv

type GetScrollDataCallbackHandler = WebView -> Int -> Int -> Int -> Int -> Int -> IO()
defGetScrollDataCallback :: GetScrollDataCallbackHandler -> GetScrollDataCallback
defGetScrollDataCallback convcb wv a1 a2 a3 a4 a0 = do
    let ar1 = fromIntegral a1
    let ar2 = fromIntegral a2
    let ar3 = fromIntegral a3
    let ar4 = fromIntegral a4
    let ar0 = fromIntegral a0
    convcb wv ar1 ar2 ar3 ar4 ar0

-- | Assign a callback function to be notified of a response to
-- 'Graphics.UI.Awesomium.WebView.requestScrollData'.
setCallbackGetScrollData :: WebView -> GetScrollDataCallbackHandler -> IO (FunPtr GetScrollDataCallback)
setCallbackGetScrollData wv ah = 
    mkGetScrollDataCallback (defGetScrollDataCallback ah) >>=>
    awe_webview_set_callback_get_scroll_data wv

type JsConsoleMessageCallbackHandler = WebView -> String -> Int -> String -> IO()
defJsConsoleMessageCallback :: JsConsoleMessageCallbackHandler -> JsConsoleMessageCallback
defJsConsoleMessageCallback convcb wv a1 a2 a0 = do
    ar1 <- fromAweString a1
    let ar2 = fromIntegral a2
    ar0 <- fromAweString a0
    convcb wv ar1 ar2 ar0

-- | Assign a callback function to be notified of any Javascript
-- console messages. (Usually Javascript errors encountered in
-- scripts)
setCallbackJsConsoleMessage :: WebView -> JsConsoleMessageCallbackHandler -> IO (FunPtr JsConsoleMessageCallback)
setCallbackJsConsoleMessage wv ah = 
    mkJsConsoleMessageCallback (defJsConsoleMessageCallback ah) >>=>
    awe_webview_set_callback_js_console_message wv

type GetFindResultsCallbackHandler = WebView -> Int -> Int -> Rect -> Int -> Bool -> IO()
defGetFindResultsCallback :: GetFindResultsCallbackHandler -> GetFindResultsCallback
defGetFindResultsCallback convcb wv a1 a2 a3 a4 a0 = do
    let ar1 = fromIntegral a1
    let ar2 = fromIntegral a2
    ar3 <- peek a3
    let ar4 = fromIntegral a4
    let ar0 = toBool a0
    convcb wv ar1 ar2 ar3 ar4 ar0

-- | Assign a callback function to be notified whenever we receive
-- results back from an in-page find operation
-- ('Graphics.UI.Awesomium.WebView.find').
setCallbackGetFindResults :: WebView -> GetFindResultsCallbackHandler -> IO (FunPtr GetFindResultsCallback)
setCallbackGetFindResults wv ah = 
    mkGetFindResultsCallback (defGetFindResultsCallback ah) >>=>
    awe_webview_set_callback_get_find_results wv

type UpdateImeCallbackHandler = WebView -> ImeState -> Rect -> IO()
defUpdateImeCallback :: UpdateImeCallbackHandler -> UpdateImeCallback
defUpdateImeCallback convcb wv a1 a0 = do
    let ar1 = toEnum . fromIntegral $ a1
    ar0 <- peek a0
    convcb wv ar1 ar0

-- | Assign a callback function to be notified whenever the user does
-- something that may change the position or visiblity of the IME
-- Widget.  This callback is only active when IME is activated (please
-- see 'Graphics.UI.Awesomium.WebView.activateIme').
setCallbackUpdateIme :: WebView -> UpdateImeCallbackHandler -> IO (FunPtr UpdateImeCallback)
setCallbackUpdateIme wv ah = 
    mkUpdateImeCallback (defUpdateImeCallback ah) >>=>
    awe_webview_set_callback_update_ime wv

type ShowContextMenuCallbackHandler = WebView -> Int -> Int -> MediaType -> Int {-MediaState-} -> String -> String -> String -> String -> String -> Bool -> Int {-CanEditFlags-} -> IO()
defShowContextMenuCallback :: ShowContextMenuCallbackHandler -> ShowContextMenuCallback
defShowContextMenuCallback convcb wv a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a0 = do
    let ar1 = fromIntegral a1
    let ar2 = fromIntegral a2
    let ar3 = toEnum . fromIntegral $ a3
    let ar4 = fromIntegral a4
    ar5 <- fromAweString a5
    ar6 <- fromAweString a6
    ar7 <- fromAweString a7
    ar8 <- fromAweString a8
    ar9 <- fromAweString a9
    let ar10 = toBool a10
    let ar0 = fromIntegral a0
    convcb wv ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar0

-- | Assign a callback function to be notified whenever the page
-- requests a context menu to be shown (usually the result of a user
-- right-clicking somewhere on the page). It is your responsiblity to
-- display a menu for the user to select an appropriate action.
setCallbackShowContextMenu :: WebView -> ShowContextMenuCallbackHandler -> IO (FunPtr ShowContextMenuCallback)
setCallbackShowContextMenu wv ah = 
    mkShowContextMenuCallback (defShowContextMenuCallback ah) >>=>
    awe_webview_set_callback_show_context_menu wv

type RequestLoginCallbackHandler = WebView -> Int -> String -> Bool -> String -> String -> String -> IO()
defRequestLoginCallback :: RequestLoginCallbackHandler -> RequestLoginCallback
defRequestLoginCallback convcb wv a1 a2 a3 a4 a5 a0 = do
    let ar1 = fromIntegral a1
    ar2 <- fromAweString a2
    let ar3 = toBool a3
    ar4 <- fromAweString a4
    ar5 <- fromAweString a5
    ar0 <- fromAweString a0
    convcb wv ar1 ar2 ar3 ar4 ar5 ar0

-- | Assign a callback function to be notified whenever a page
-- requests authentication from the user (ex, Basic HTTP Auth, NTLM
-- Auth, etc.).  See 'Graphics.UI.Awesomium.WebView.login' and
-- 'Graphics.UI.Awesomium.WebView.cancelLogin'.
setCallbackRequestLogin :: WebView -> RequestLoginCallbackHandler -> IO (FunPtr RequestLoginCallback)
setCallbackRequestLogin wv ah = 
    mkRequestLoginCallback (defRequestLoginCallback ah) >>=>
    awe_webview_set_callback_request_login wv

type ChangeHistoryCallbackHandler = WebView -> Int -> Int -> IO()
defChangeHistoryCallback :: ChangeHistoryCallbackHandler -> ChangeHistoryCallback
defChangeHistoryCallback convcb wv a1 a0 = do
    let ar1 = fromIntegral a1
    let ar0 = fromIntegral a0
    convcb wv ar1 ar0

-- | Assign a callback function to be notified whenever the history
-- state has changed. (eg, the state of thie back/forward buttons
-- should be updated)
setCallbackChangeHistory :: WebView -> ChangeHistoryCallbackHandler -> IO (FunPtr ChangeHistoryCallback)
setCallbackChangeHistory wv ah = 
    mkChangeHistoryCallback (defChangeHistoryCallback ah) >>=>
    awe_webview_set_callback_change_history wv

type FinishResizeCallbackHandler = WebView -> Int -> Int -> IO()
defFinishResizeCallback :: FinishResizeCallbackHandler -> FinishResizeCallback
defFinishResizeCallback convcb wv a1 a0 = do
    let ar1 = fromIntegral a1
    let ar0 = fromIntegral a0
    convcb wv ar1 ar0

-- | Assign a callback function to be notified whenever a WebView has
-- finished resizing to a certain size (and has finished repainting
-- the RenderBuffer).
setCallbackFinishResize :: WebView -> FinishResizeCallbackHandler -> IO (FunPtr FinishResizeCallback)
setCallbackFinishResize wv ah = 
    mkFinishResizeCallback (defFinishResizeCallback ah) >>=>
    awe_webview_set_callback_finish_resize wv

type ShowJavascriptDialogCallbackHandler = WebView -> Int -> Int {-DialogFlags-} -> String -> String -> String -> IO()
defShowJavascriptDialogCallback :: ShowJavascriptDialogCallbackHandler -> ShowJavascriptDialogCallback
defShowJavascriptDialogCallback convcb wv a1 a2 a3 a4 a0 = do
    let ar1 = fromIntegral a1
    let ar2 = fromIntegral a2
    ar3 <- fromAweString a3
    ar4 <- fromAweString a4
    ar0 <- fromAweString a0
    convcb wv ar1 ar2 ar3 ar4 ar0

-- | Assign a callback function to be notified whenever a WebView
-- requests that a certain Javascript dialog be shown (eg, alert,
-- confirm, prompt). See
-- 'Graphics.UI.Awesomium.WebView.closeJavascriptDialog' for more
-- information.
setCallbackShowJavascriptDialog :: WebView -> ShowJavascriptDialogCallbackHandler -> IO (FunPtr ShowJavascriptDialogCallback)
setCallbackShowJavascriptDialog wv ah = 
    mkShowJavascriptDialogCallback (defShowJavascriptDialogCallback ah) >>=>
    awe_webview_set_callback_show_javascript_dialog wv

