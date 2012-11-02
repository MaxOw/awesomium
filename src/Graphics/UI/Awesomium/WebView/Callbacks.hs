module Graphics.UI.Awesomium.WebView.Callbacks
    ( BeginNavigationCallbackHandler      , setCallbackBeginNavigation
    , BeginLoadingCallbackHandler         , setCallbackBeginLoading
    , FinishLoadingCallbackHandler        , setCallbackFinishLoading
    , JSCallbackHandler                   , setCallbackJS
    , ReceiveTitleCallbackHandler         , setCallbackReceiveTitle
    , ChangeTooltipCallbackHandler        , setCallbackChangeTooltip
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
    , UpdateImeCallbackHandler            , setCallbackUpdateIme
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

setCallbackBeginLoading :: WebView -> BeginLoadingCallbackHandler -> IO (FunPtr BeginLoadingCallback)
setCallbackBeginLoading wv ah = 
    mkBeginLoadingCallback (defBeginLoadingCallback ah) >>=>
    awe_webview_set_callback_begin_loading wv

type FinishLoadingCallbackHandler = WebView -> IO()
defFinishLoadingCallback :: FinishLoadingCallbackHandler -> FinishLoadingCallback
defFinishLoadingCallback convcb wv = do
    convcb wv

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

setCallbackReceiveTitle :: WebView -> ReceiveTitleCallbackHandler -> IO (FunPtr ReceiveTitleCallback)
setCallbackReceiveTitle wv ah = 
    mkReceiveTitleCallback (defReceiveTitleCallback ah) >>=>
    awe_webview_set_callback_receive_title wv

type ChangeTooltipCallbackHandler = WebView -> String -> IO()
defChangeTooltipCallback :: ChangeTooltipCallbackHandler -> ChangeTooltipCallback
defChangeTooltipCallback convcb wv a0 = do
    ar0 <- fromAweString a0
    convcb wv ar0

setCallbackChangeTooltip :: WebView -> ChangeTooltipCallbackHandler -> IO (FunPtr ChangeTooltipCallback)
setCallbackChangeTooltip wv ah = 
    mkChangeTooltipCallback (defChangeTooltipCallback ah) >>=>
    awe_webview_set_callback_change_tooltip wv

type ChangeCursorCallbackHandler = WebView -> CursorType -> IO()
defChangeCursorCallback :: ChangeCursorCallbackHandler -> ChangeCursorCallback
defChangeCursorCallback convcb wv a0 = do
    let ar0 = toEnum . fromIntegral $ a0
    convcb wv ar0

setCallbackChangeCursor :: WebView -> ChangeCursorCallbackHandler -> IO (FunPtr ChangeCursorCallback)
setCallbackChangeCursor wv ah = 
    mkChangeCursorCallback (defChangeCursorCallback ah) >>=>
    awe_webview_set_callback_change_cursor wv

type ChangeKeyboardFocusCallbackHandler = WebView -> Bool -> IO()
defChangeKeyboardFocusCallback :: ChangeKeyboardFocusCallbackHandler -> ChangeKeyboardFocusCallback
defChangeKeyboardFocusCallback convcb wv a0 = do
    let ar0 = toBool a0
    convcb wv ar0

setCallbackChangeKeyboardFocus :: WebView -> ChangeKeyboardFocusCallbackHandler -> IO (FunPtr ChangeKeyboardFocusCallback)
setCallbackChangeKeyboardFocus wv ah = 
    mkChangeKeyboardFocusCallback (defChangeKeyboardFocusCallback ah) >>=>
    awe_webview_set_callback_change_keyboard_focus wv

type ChangeTargetUrlCallbackHandler = WebView -> String -> IO()
defChangeTargetUrlCallback :: ChangeTargetUrlCallbackHandler -> ChangeTargetUrlCallback
defChangeTargetUrlCallback convcb wv a0 = do
    ar0 <- fromAweString a0
    convcb wv ar0

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

setCallbackOpenExternalLink :: WebView -> OpenExternalLinkCallbackHandler -> IO (FunPtr OpenExternalLinkCallback)
setCallbackOpenExternalLink wv ah = 
    mkOpenExternalLinkCallback (defOpenExternalLinkCallback ah) >>=>
    awe_webview_set_callback_open_external_link wv

type RequestDownloadCallbackHandler = WebView -> String -> IO()
defRequestDownloadCallback :: RequestDownloadCallbackHandler -> RequestDownloadCallback
defRequestDownloadCallback convcb wv a0 = do
    ar0 <- fromAweString a0
    convcb wv ar0

setCallbackRequestDownload :: WebView -> RequestDownloadCallbackHandler -> IO (FunPtr RequestDownloadCallback)
setCallbackRequestDownload wv ah = 
    mkRequestDownloadCallback (defRequestDownloadCallback ah) >>=>
    awe_webview_set_callback_request_download wv

type WebViewCrashedCallbackHandler = WebView -> IO()
defWebViewCrashedCallback :: WebViewCrashedCallbackHandler -> WebViewCrashedCallback
defWebViewCrashedCallback convcb wv = do
    convcb wv

setCallbackWebViewCrashed :: WebView -> WebViewCrashedCallbackHandler -> IO (FunPtr WebViewCrashedCallback)
setCallbackWebViewCrashed wv ah = 
    mkWebViewCrashedCallback (defWebViewCrashedCallback ah) >>=>
    awe_webview_set_callback_web_view_crashed wv

type PluginCrashedCallbackHandler = WebView -> String -> IO()
defPluginCrashedCallback :: PluginCrashedCallbackHandler -> PluginCrashedCallback
defPluginCrashedCallback convcb wv a0 = do
    ar0 <- fromAweString a0
    convcb wv ar0

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

setCallbackGetPageContents :: WebView -> GetPageContentsCallbackHandler -> IO (FunPtr GetPageContentsCallback)
setCallbackGetPageContents wv ah = 
    mkGetPageContentsCallback (defGetPageContentsCallback ah) >>=>
    awe_webview_set_callback_get_page_contents wv

type DomReadyCallbackHandler = WebView -> IO()
defDomReadyCallback :: DomReadyCallbackHandler -> DomReadyCallback
defDomReadyCallback convcb wv = do
    convcb wv

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

setCallbackShowJavascriptDialog :: WebView -> ShowJavascriptDialogCallbackHandler -> IO (FunPtr ShowJavascriptDialogCallback)
setCallbackShowJavascriptDialog wv ah = 
    mkShowJavascriptDialogCallback (defShowJavascriptDialogCallback ah) >>=>
    awe_webview_set_callback_show_javascript_dialog wv

