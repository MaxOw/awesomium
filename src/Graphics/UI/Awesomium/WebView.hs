module Graphics.UI.Awesomium.WebView
    ( WebView, destroy, loadUrl, loadHtml, loadFile, getUrl
    , goToHistoryOffset, getHistoryBackCount, getHistoryForwardCount
    , stop, reload, executeJavascript, executeJavascriptWithResult
    , callJavascriptFunction, createObject, destroyObject
    , setObjectProperty, setObjectCallback, isLoadingPage, isDirty
    , render, pauseRendering, resumeRendering, MouseButton (..)
    , injectMouseMove, injectMouseDown, injectMouseUp
    , injectMouseWheel, cut, copy, paste, selectAll, copyImageAt
    , setZoom, resetZoom, getZoom, getZoomForHost, resize
    , isResizing, unfocus, focus, setTransparent, isTransparent
    , setUrlFilteringMode, addUrlFilter, clearAllUrlFilters
    , addHeaderRewriteRule, removeHeaderRewriteRule
    , removeHeaderRewriteRulesByDefinitionName, chooseFile, print
    , requestScrollData, find, stopFind, translatePage, activateIme
    , setImeComposition, confirmImeComposition, cancelImeComposition
    , login, cancelLogin, closeJavascriptDialog
    
    , module Graphics.UI.Awesomium.WebView.Callbacks
) where

import Prelude hiding (print)
import Foreign.Ptr (FunPtr)
--import Control.Monad (Monad)
import Graphics.UI.Awesomium.Raw
import Graphics.UI.Awesomium.WebView.Callbacks

destroy :: WebView -> IO ()
destroy = awe_webview_destroy

loadUrl :: WebView -> String -> String -> String -> String -> IO ()
loadUrl = awe_webview_load_url

loadHtml :: WebView -> String -> String -> IO ()
loadHtml = awe_webview_load_html

loadFile :: WebView -> String -> String -> IO ()
loadFile = awe_webview_load_file

getUrl :: WebView -> IO String
getUrl = awe_webview_get_url

goToHistoryOffset :: WebView -> Int -> IO ()
goToHistoryOffset = awe_webview_go_to_history_offset

getHistoryBackCount :: WebView -> IO Int
getHistoryBackCount = awe_webview_get_history_back_count

getHistoryForwardCount :: WebView -> IO Int
getHistoryForwardCount = awe_webview_get_history_forward_count

stop :: WebView -> IO ()
stop = awe_webview_stop

reload :: WebView -> IO ()
reload = awe_webview_reload

executeJavascript :: WebView -> String -> String -> IO ()
executeJavascript = awe_webview_execute_javascript

executeJavascriptWithResult :: WebView -> String -> String -> Int -> IO JSValue
executeJavascriptWithResult = awe_webview_execute_javascript_with_result

callJavascriptFunction :: WebView -> String -> String -> JSArray -> String -> IO ()
callJavascriptFunction = awe_webview_call_javascript_function

createObject :: WebView -> String -> IO ()
createObject = awe_webview_create_object

destroyObject :: WebView -> String -> IO ()
destroyObject = awe_webview_destroy_object

setObjectProperty :: WebView -> String -> String -> JSValue -> IO ()
setObjectProperty = awe_webview_set_object_property

setObjectCallback :: WebView -> String -> String -> IO ()
setObjectCallback = awe_webview_set_object_callback

isLoadingPage :: WebView -> IO Bool
isLoadingPage = awe_webview_is_loading_page

isDirty :: WebView -> IO Bool
isDirty = awe_webview_is_dirty

-- get_dirty_bounds = awe_webview_get_dirty_bounds :: WebView -> rect

render :: WebView -> IO RenderBuffer
render = awe_webview_render

pauseRendering :: WebView -> IO ()
pauseRendering = awe_webview_pause_rendering

resumeRendering :: WebView -> IO ()
resumeRendering = awe_webview_resume_rendering

injectMouseMove :: WebView -> Int -> Int -> IO ()
injectMouseMove = awe_webview_inject_mouse_move

injectMouseDown :: WebView -> MouseButton -> IO ()
injectMouseDown = awe_webview_inject_mouse_down

injectMouseUp :: WebView -> MouseButton -> IO ()
injectMouseUp = awe_webview_inject_mouse_up

injectMouseWheel :: WebView -> Int -> Int -> IO ()
injectMouseWheel = awe_webview_inject_mouse_wheel

-- inject_keyboard_event = awe_webview_inject_keyboard_event :: WebView -> webkeyboardevent -> IO ()
-- #ifdef _WIN32
-- inject_keyboard_event_win = awe_webview_inject_keyboard_event_win :: WebView', UINT msg, WPARAM wparam, LPARAM lparam } -> IO ()
-- #endif

cut :: WebView -> IO ()
cut = awe_webview_cut

copy :: WebView -> IO ()
copy = awe_webview_copy

paste :: WebView -> IO ()
paste = awe_webview_paste

selectAll :: WebView -> IO ()
selectAll = awe_webview_select_all

copyImageAt :: WebView -> Int -> Int -> IO ()
copyImageAt = awe_webview_copy_image_at

setZoom :: WebView -> Int -> IO ()
setZoom = awe_webview_set_zoom

resetZoom :: WebView -> IO ()
resetZoom = awe_webview_reset_zoom

getZoom :: WebView -> IO Int
getZoom = awe_webview_get_zoom

getZoomForHost :: WebView -> String -> IO Int
getZoomForHost = awe_webview_get_zoom_for_host

resize :: WebView -> Int -> Int -> Bool -> Int -> IO Bool
resize = awe_webview_resize

isResizing :: WebView -> IO Bool
isResizing = awe_webview_is_resizing

unfocus :: WebView -> IO ()
unfocus = awe_webview_unfocus

focus :: WebView -> IO ()
focus = awe_webview_focus

setTransparent :: WebView -> Bool -> IO ()
setTransparent = awe_webview_set_transparent

isTransparent :: WebView -> IO Bool
isTransparent = awe_webview_is_transparent

setUrlFilteringMode :: WebView -> UrlFilteringMode -> IO ()
setUrlFilteringMode = awe_webview_set_url_filtering_mode

addUrlFilter :: WebView -> String -> IO ()
addUrlFilter = awe_webview_add_url_filter

clearAllUrlFilters :: WebView -> IO ()
clearAllUrlFilters = awe_webview_clear_all_url_filters

-- TODO: webview_set_header_definition :: WebView -> String -> [(String, String)] -> IO ()
-- set_header_definition = awe_webview_set_header_definition :: WebView -> String -> Int -> Ptr AweString -> Ptr AweString -> IO ()

addHeaderRewriteRule :: WebView -> String -> String -> IO ()
addHeaderRewriteRule = awe_webview_add_header_rewrite_rule

removeHeaderRewriteRule :: WebView -> String -> IO ()
removeHeaderRewriteRule = awe_webview_remove_header_rewrite_rule

removeHeaderRewriteRulesByDefinitionName :: WebView -> String -> IO ()
removeHeaderRewriteRulesByDefinitionName = awe_webview_remove_header_rewrite_rules_by_definition_name

chooseFile :: WebView -> String -> IO ()
chooseFile = awe_webview_choose_file

print :: WebView -> IO ()
print = awe_webview_print

requestScrollData :: WebView -> String -> IO ()
requestScrollData = awe_webview_request_scroll_data

find :: WebView -> Int -> String -> Bool -> Bool -> Bool -> IO ()
find = awe_webview_find

stopFind :: WebView -> Bool -> IO ()
stopFind = awe_webview_stop_find

translatePage :: WebView -> String -> String -> IO ()
translatePage = awe_webview_translate_page

activateIme :: WebView -> Bool -> IO ()
activateIme = awe_webview_activate_ime

setImeComposition :: WebView -> String -> Int -> Int -> Int -> IO ()
setImeComposition = awe_webview_set_ime_composition

confirmImeComposition :: WebView -> String -> IO ()
confirmImeComposition = awe_webview_confirm_ime_composition

cancelImeComposition :: WebView -> IO ()
cancelImeComposition = awe_webview_cancel_ime_composition

login :: WebView -> Int -> String -> String -> IO ()
login = awe_webview_login

cancelLogin :: WebView -> Int -> IO ()
cancelLogin = awe_webview_cancel_login

closeJavascriptDialog :: WebView -> Int -> Bool -> String -> IO ()
closeJavascriptDialog = awe_webview_close_javascript_dialog

