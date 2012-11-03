module Graphics.UI.Awesomium.WebView
    ( WebView, Rect, WebkeyboardEvent, destroy, loadUrl, loadHtml
    , loadFile, getUrl, goToHistoryOffset, getHistoryBackCount
    , getHistoryForwardCount, stop, reload, executeJavascript
    , executeJavascriptWithResult, callJavascriptFunction
    , createObject, destroyObject, setObjectProperty
    , setObjectCallback, isLoadingPage, isDirty, getDirtyBounds
    , render, pauseRendering, resumeRendering, MouseButton (..)
    , injectMouseMove, injectMouseDown, injectMouseUp
    , injectMouseWheel, injectKeyboardEvent, cut, copy, paste
    , selectAll, copyImageAt, setZoom, resetZoom, getZoom
    , getZoomForHost, resize, isResizing, unfocus, focus
    , setTransparent, isTransparent, UrlFilteringMode (..)
    , setUrlFilteringMode, addUrlFilter, clearAllUrlFilters
    , setHeaderDefinition, addHeaderRewriteRule
    , removeHeaderRewriteRule
    , removeHeaderRewriteRulesByDefinitionName, chooseFile, print
    , requestScrollData, find, stopFind, translatePage, activateIme
    , setImeComposition, confirmImeComposition, cancelImeComposition
    , login, cancelLogin, closeJavascriptDialog

    , module Graphics.UI.Awesomium.WebView.Callbacks
) where

import Graphics.UI.Awesomium.Raw
import Graphics.UI.Awesomium.Javascript
import Graphics.UI.Awesomium.WebView.Callbacks

import Prelude hiding (print)
import Foreign.Ptr (FunPtr)
import Data.Aeson

-- | Queue a WebView for destruction by the WebCore.
destroy
    :: WebView -- ^ The WebView instance.
    -> IO ()
destroy = awe_webview_destroy

-- | Loads a URL into the WebView asynchronously.
loadUrl
    :: WebView -- ^ The WebView instance.
    -> String -- ^ The URL to load.
    -> String -- ^ The name of the frame to load the URL
    -- in; leave this blank to load in the main frame.
    -> String -- ^ If the URL requires authentication, the username
    -- to authorize as, otherwise just pass an empty string.
    -> String -- ^ If the URL requires authentication, the password
    -- to use, otherwise just pass an empty string.
    -> IO ()
loadUrl = awe_webview_load_url

-- | Loads a string of HTML into the WebView asynchronously.
loadHtml
    :: WebView -- ^ The WebView instance.
    -> String -- ^ The HTML string (ASCII) to load.
    -> String -- ^ The name of the frame to load the HTML
    -- in; leave this blank to load in the main frame.
    -> IO ()
loadHtml = awe_webview_load_html

-- | Loads a local file into the WebView asynchronously.
--
-- The file should exist within the base directory.
loadFile
    :: WebView -- ^ The WebView instance.
    -> String -- ^ The file to load.
    -> String -- ^ The name of the frame to load the file
    -- in; leave this blank to load in the main frame.
    -> IO ()
loadFile = awe_webview_load_file

getUrl
    :: WebView -- ^ The WebView instance.
    -> IO String -- ^ URL
getUrl = awe_webview_get_url

-- | Navigates back/forward in history via a relative offset.
goToHistoryOffset
    :: WebView -- ^ The WebView instance.
    -> Int -- ^ The relative offset in history to navigate to.
    -> IO ()
goToHistoryOffset = awe_webview_go_to_history_offset

-- | Get the number of steps back in history we can go.
getHistoryBackCount
    :: WebView
    -> IO Int
getHistoryBackCount = awe_webview_get_history_back_count

-- | Get the number of steps forward in history we can go.
getHistoryForwardCount
    :: WebView
    -> IO Int
getHistoryForwardCount = awe_webview_get_history_forward_count

-- | Stops the current navigation.
stop
    :: WebView
    -> IO ()
stop = awe_webview_stop

-- | Reloads the current page.
reload
    :: WebView
    -> IO ()
reload = awe_webview_reload

-- | Executes a string of Javascript in the context of the current
-- page asynchronously.
executeJavascript
    :: WebView
    -> String -- ^ The string of Javascript to execute.
    -> String -- ^ The name of the frame to execute in;
    -- pass an empty string to execute in the main frame.
    -> IO ()
executeJavascript = awe_webview_execute_javascript

-- | Executes a string of Javascript in the context of the current
-- page asynchronously with a result.
executeJavascriptWithResult
    :: WebView
    -> String -- ^ The string of Javascript to execute.
    -> String -- ^ The name of the frame to execute in;
    -- pass an empty string to execute in the main frame.
    -> Int -- ^ The maximum amount of time (in milliseconds) to wait
    -- for a result. Pass 0 to use no timeout. (If no result
    -- is obtained, or the timeout is reached, this function
    -- will return a 'Value' with Null)
    -> IO Value -- ^ Returns an 'Value'.
executeJavascriptWithResult wv o fn to = jsvalueToJSON =<<
    awe_webview_execute_javascript_with_result wv o fn to

-- | Call a certain function defined in Javascript directly.
callJavascriptFunction
    :: WebView
    -> String -- ^ The name of the object that contains the function,
    -- pass an empty string if the function is defined in
    -- the global scope.
    -> String -- ^ The name of the function.
    -> [Value] -- ^ The arguments to pass to the function.
    -> String -- ^ The name of the frame to execute in;
    -- leave this blank to execute in the main frame.
    -> IO ()
callJavascriptFunction wv o fn arg fr =
    withJSArray arg $ \arg' ->
    awe_webview_call_javascript_function wv o fn arg' fr

-- | Creates a new global Javascript object that will persist
-- throughout the lifetime of this WebView. This object is managed
-- directly by Awesomium and so you can modify its properties and bind
-- callback functions via 'setObjectProperty' and 'setObjectCallback',
-- respectively.
createObject
    :: WebView
    -> String -- ^ The name of the object.
    -> IO ()
createObject = awe_webview_create_object

-- | Destroys a Javascript object previously created by
-- 'createObject'
destroyObject
    :: WebView
    -> String -- ^ The name of the object to destroy.
    -> IO ()
destroyObject = awe_webview_destroy_object

-- | Sets a property of a Javascript object previously created by
-- 'createObject'.
setObjectProperty
    :: WebView
    -> String -- ^ The name of the Javascript object.
    -> String -- ^ The name of the property.
    -> Value -- ^ The javascript value of the property.
    -> IO ()
setObjectProperty wv o fn v = 
    withJS v $ \v' ->
    awe_webview_set_object_property wv o fn v'

-- | Sets a callback function of a Javascript object previously
-- created by 'createObject'. This is very useful for passing events
-- from Javascript to Haskell. To receive notification of the
-- callback, please see 'setCallbackJS'.
setObjectCallback
    :: WebView
    -> String -- ^ The name of the Javascript object.
    -> String -- ^ The name of the callback function.
    -> IO ()
setObjectCallback = awe_webview_set_object_callback

-- | Returns whether or not a page is currently loading in the
-- WebView.
isLoadingPage
    :: WebView
    -> IO Bool -- ^ If a page is loading, returns True, otherwise
    -- returns False.
isLoadingPage = awe_webview_is_loading_page

-- | Returns whether or not the WebView is dirty and needs to be
-- re-rendered via 'render'.
isDirty
    :: WebView
    -> IO Bool -- ^ the WebView is dirty, returns True, otherwise
    -- returns False.
isDirty = awe_webview_is_dirty

-- | Returns the bounds of the area that has changed since the last
-- call to 'render'.
getDirtyBounds
    :: WebView
    -> IO Rect -- ^ bounds of the dirty area.
getDirtyBounds = awe_webview_get_dirty_bounds

-- | Renders this WebView into an offscreen render buffer and clears
-- the dirty state.
render
    :: WebView
    -> IO RenderBuffer -- ^ A pointer to the internal render buffer
    -- instance that was used to render this WebView. This value may
    -- change between renders and may return 'nullPtr' if the WebView
    -- has crashed.
render = awe_webview_render

-- | All rendering is actually done asynchronously in a separate
-- process and so the page is usually continuously rendering even if
-- you never call 'render'. Call this to temporarily pause rendering.
pauseRendering :: WebView -> IO ()
pauseRendering = awe_webview_pause_rendering

-- | Resume rendering after all call to pause_rendering.
resumeRendering :: WebView -> IO ()
resumeRendering = awe_webview_resume_rendering

-- | Injects a mouse-move event in local coordinates.
injectMouseMove
    :: WebView
    -> Int -- ^ The absolute x-coordinate of the mouse (localized to
    -- the WebView).
    -> Int -- ^ The absolute y-coordinate of the mouse (localized to
    -- the WebView).
    -> IO ()
injectMouseMove = awe_webview_inject_mouse_move

-- | Injects a mouse-down event.
injectMouseDown
    :: WebView
    -> MouseButton -- ^ The button that was pressed.
    -> IO ()
injectMouseDown = awe_webview_inject_mouse_down

-- | Injects a mouse-up event.
injectMouseUp
    :: WebView
    -> MouseButton -- ^ The button that was released.
    -> IO ()
injectMouseUp = awe_webview_inject_mouse_up

-- | Injects a mouse-wheel event.
injectMouseWheel
    :: WebView
    -> Int -- ^ The relative amount of pixels to scroll vertically.
    -> Int -- ^ The relative amount of pixels to scroll horizontally.
    -> IO ()
injectMouseWheel = awe_webview_inject_mouse_wheel

-- | Injects a keyboard event. You'll need to initialize the members
-- of 'WebkeyboardEvent' yourself.
injectKeyboardEvent
    :: WebView
    -> WebkeyboardEvent -- ^ The keyboard event to inject.
    -> IO ()
injectKeyboardEvent = awe_webview_inject_keyboard_event

--)| Injects a native Windows keyboard event.
--)^ The msg parameter.
--)^ The wparam parameter.
--)^ The lparam parameter.
-- #ifdef _WIN32
-- inject_keyboard_event_win = awe_webview_inject_keyboard_event_win
--  :: WebView', UINT msg, WPARAM wparam, LPARAM lparam }
--  -> IO ()
-- #endif

-- | Invokes a 'cut' action using the system clipboard.
cut :: WebView -> IO ()
cut = awe_webview_cut

-- | Invokes a 'copy' action using the system clipboard.
copy :: WebView -> IO ()
copy = awe_webview_copy

-- | Invokes a 'paste' action using the system clipboard.
paste :: WebView -> IO ()
paste = awe_webview_paste

-- | Selects all items on the current page.
selectAll :: WebView -> IO ()
selectAll = awe_webview_select_all

-- | Copies an image on the page to the system clipboard.
copyImageAt
    :: WebView
    -> Int -- ^ x
    -> Int -- ^ y
    -> IO ()
copyImageAt = awe_webview_copy_image_at

-- | Zooms the page a specified percent.
setZoom
    :: WebView
    -> Int -- ^ The percent of the page to zoom to. Valid range
    -- is from 10% to 500%.
    -> IO ()
setZoom = awe_webview_set_zoom

-- | Resets the zoom level.
resetZoom
    :: WebView
    -> IO ()
resetZoom = awe_webview_reset_zoom

-- | Gets the current zoom level.
getZoom :: WebView -> IO Int
getZoom = awe_webview_get_zoom

-- | Gets the zoom level for a specific hostname.
getZoomForHost :: WebView -> String -> IO Int
getZoomForHost = awe_webview_get_zoom_for_host

-- | Resizes this WebView to certain dimensions.
resize
    :: WebView
    -> Int -- ^ The width in pixels to resize to.
    -> Int -- ^ The height in pixels to resize to.
    -> Bool -- ^ Whether or not to wait for the WebView
    -- to finish repainting.
    -> Int -- ^ The maximum amount of time to wait
    -- for a repaint, in milliseconds.
    -> IO Bool -- ^ true if the resize was successful. This operation
    -- can fail if there is another resize already pending (see
    -- 'isResizing') or if the repaint timeout was exceeded.
resize = awe_webview_resize

-- | Checks whether or not there is a resize operation pending.
isResizing
    :: WebView
    -> IO Bool -- ^ true if we are waiting for the WebView process to
    -- return acknowledgement of a pending resize operation.
isResizing = awe_webview_is_resizing

-- | Notifies the current page that it has lost focus.
unfocus :: WebView -> IO ()
unfocus = awe_webview_unfocus

-- | Notifies the current page that is has gained focus. You will need
-- to call this to gain textbox focus, among other things. (If you
-- fail to ever see a blinking caret when typing text, this is why).
focus :: WebView -> IO ()
focus = awe_webview_focus

-- | Sets whether or not pages should be rendered with transparency
-- preserved. (ex, for pages with
-- style=/"background-color:transparent"/)
setTransparent
    :: WebView
    -> Bool -- ^ Whether or not this WebView is transparent.
    -> IO ()
setTransparent = awe_webview_set_transparent

isTransparent :: WebView -> IO Bool
isTransparent = awe_webview_is_transparent

-- | Sets the current URL Filtering Mode (default is UfmNone).
-- See 'UrlFilteringMode' for more information on the modes.
setUrlFilteringMode
    :: WebView
    -> UrlFilteringMode -- ^ The URL filtering mode to use.
    -> IO ()
setUrlFilteringMode = awe_webview_set_url_filtering_mode

-- | Adds a new URL Filter rule.
--
-- [@note@] For example, to match all URLs from the domain
-- @\"google.com\"@, your filter string might be:
-- @http:\/\/google.com\/*@
--
-- [@note2@] You may also use the @\"local:\/\/\"@ scheme prefix to
-- describe the URL to the base directory (set via
-- 'WebCore.setBaseDirectory').
--
addUrlFilter
    :: WebView
    -> String -- ^ A string with optional wildcards that describes
    -- a certain URL.
    -> IO ()
addUrlFilter = awe_webview_add_url_filter

-- | Clears all URL Filter rules.
clearAllUrlFilters :: WebView -> IO ()
clearAllUrlFilters = awe_webview_clear_all_url_filters

-- | Defines a new Header Definition or updates it if it already
-- exists.
setHeaderDefinition
    :: WebView
    -> String -- ^ The unique name of the Header Definition; this is
    -- used to refer to it later in 'addHeaderRewriteRule' and related
    -- methods.
    -> [(String, String)] -- ^ An array of string pairs
    -- (field name, field value)
    -> IO ()
setHeaderDefinition = awe_webview_set_header_definition

-- | Adds a new a header re-write rule. All requests whose URL matches
-- the specified rule will have its  HTTP headers re-written with the
-- specified header definition before sending it to the server.
--
-- [@note@] The case where a URL is matched by multiple rules is
-- unsupported, only the first match will be used.
addHeaderRewriteRule
    :: WebView
    -> String -- ^ A string with optional wildcards (*, ?) that
    -- matches the URL(s) that will have its headers
    -- re-written with the specified header definition.
    -> String -- ^ The name of the header definition (specified in
    -- 'setHeaderDefinition').
    -> IO ()
addHeaderRewriteRule = awe_webview_add_header_rewrite_rule

-- | Removes a header re-write rule from this WebView.

removeHeaderRewriteRule
    :: WebView
    -> String -- ^ The rule to remove (should match the string
    -- specified in 'addHeaderRewriteRule' exactly).
    -> IO ()
removeHeaderRewriteRule = awe_webview_remove_header_rewrite_rule

-- | Removes all header re-write rules that are using a certain header
-- definition.
removeHeaderRewriteRulesByDefinitionName
    :: WebView
    -> String -- ^ The name of the header definition (specified in
    -- 'setHeaderDefinition'). If you specify an
    -- empty string, this will remove ALL header re-write rules.
    -> IO ()
removeHeaderRewriteRulesByDefinitionName =
    awe_webview_remove_header_rewrite_rules_by_definition_name

-- | This should be called as a response to the request file chooser
-- callback.
chooseFile
    :: WebView
    -> String -- ^ The full path to the file that was chosen.
    -> IO ()
chooseFile = awe_webview_choose_file

-- | Print the current page. To suppress the printer selection dialog
-- and print immediately using the operating system's defaults, see
-- 'WebCore.setSuppressPrinterDialog'.
print
    :: WebView
    -> IO ()
print = awe_webview_print

-- | Request the page dimensions and scroll position of the page. You
-- can retrieve the response via the get scroll data callback.
requestScrollData
    :: WebView
    -> String -- ^ The frame's scroll data to retrieve. Leave blank
    -- to get the main frame's scroll data.
    -> IO ()
requestScrollData = awe_webview_request_scroll_data

-- | Start finding a certain string on the current web-page. All
-- matches of the string will be highlighted on the page and you can
-- jump to different instances of the string by using the /Find Next/
-- parameter. To get actual stats about a certain query, please see
-- 'setCallbackGetFindResults'.
find
    :: WebView
    -> Int -- ^ A unique numeric ID for each search. You will
    -- need to generate one yourself for each unique
    -- search. Please note that you should use the
    -- same request id if you wish to iterate through
    -- all the search results using the /Find Next/
    -- parameter.
    -> String -- ^ The string to search for.
    -> Bool -- ^ Whether or not we should search forward, down
    -- the page.
    -> Bool -- ^ Whether or not this search is case-sensitive.
    -> Bool -- ^ /Find Next/ - Whether or not we should jump to the
    -- next instance of a search string (you should use the same
    -- request id as a previously-successful search).
    -> IO ()
find = awe_webview_find

-- | Stop finding. This will un-highlight all matches of a previous
-- call to 'find'.
stopFind
    :: WebView
    -> Bool -- ^ Whether or not we should also deselect
    -- the currently-selected string instance.
    -> IO ()
stopFind = awe_webview_stop_find

-- | Attempt automatic translation of the current page via Google
-- Translate. All language codes are ISO 639-2.
translatePage
    :: WebView
    -> String -- ^ The language to translate from
    -- (for ex. "en" for English)
    -> String -- ^ The language to translate to
    -- (for ex. "fr" for French)
    -> IO ()
translatePage = awe_webview_translate_page

-- | Call this method to let the WebView know you will be passing
-- text input via IME and will need to be notified of any
-- IME-related events (caret position, user unfocusing textbox, etc.)
-- Please see 'setCallbackUpdateIme
activateIme
    :: WebView
    -> Bool
    -> IO ()
activateIme = awe_webview_activate_ime

-- | Update the current IME text composition.
setImeComposition
    :: WebView
    -> String -- ^ The string generated by your IME.
    -> Int -- ^ The current cursor position in your IME composition.
    -> Int -- ^ The position of the beginning of the selection.
    -> Int -- ^ The position of the end of the selection.
    -> IO ()
setImeComposition = awe_webview_set_ime_composition

-- | Confirm a current IME text composition.
confirmImeComposition
    :: WebView
    -> String -- ^ The string generated by your IME.
    -> IO ()
confirmImeComposition = awe_webview_confirm_ime_composition

-- | Cancel a current IME text composition.
cancelImeComposition
    :: WebView
    -> IO ()
cancelImeComposition = awe_webview_cancel_ime_composition

-- | Respond to the "request login" callback with some user-supplied
-- credentials.
login
    :: WebView
    -> Int -- ^ The unique ID of the request.
    -> String -- ^ The username supplied by the user.
    -> String -- ^ The password supplied by the user.
    -> IO ()
login = awe_webview_login

-- | Respond to the "request login" callback by telling the
-- server that the user cancelled the authentication request.
cancelLogin
    :: WebView
    -> Int -- ^ The unique ID of the request.
    -> IO ()
cancelLogin = awe_webview_cancel_login

-- | Respond to the "show javascript dialog" callback.
closeJavascriptDialog
    :: WebView
    -> Int -- ^ The unique ID of the dialog request.
    -> Bool -- ^ Whether or not the dialog was cancelled/ignored.
    -> String -- ^ If the dialog had a prompt, you should pass
    -- whatever text the user entered into the textbox via this
    -- parameter.
    -> IO ()
closeJavascriptDialog = awe_webview_close_javascript_dialog
