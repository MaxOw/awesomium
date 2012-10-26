module Graphics.UI.Awesomium.RenderBuffer
    ( RenderBuffer
    
    , getWidth
    , getHeight
    , getRowspan
    , getBuffer
 -- , copyTo
 -- , copyToFloat
    , saveToPng
    , saveToJpeg
    , getAlphaAtPoint
    , flushAlpha
) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CUChar)
import Graphics.UI.Awesomium.Raw

getWidth :: RenderBuffer -> IO Int
getWidth = awe_renderbuffer_get_width

getHeight :: RenderBuffer -> IO Int
getHeight = awe_renderbuffer_get_height

getRowspan :: RenderBuffer -> IO Int
getRowspan = awe_renderbuffer_get_rowspan

getBuffer :: RenderBuffer -> IO (Ptr CUChar)
getBuffer = awe_renderbuffer_get_buffer

-- copyTo :: RenderBuffer -> (unsigned char* dest_buffer) -> Int -> Int -> Bool -> Bool -> IO ()
-- copyTo = awe_renderbuffer_copy_to

-- copyToFloat :: RenderBuffer -> (float* dest_buffer) -> IO ()
-- copyToFloat = awe_renderbuffer_copy_to_float

saveToPng :: RenderBuffer -> String -> Bool -> IO Bool
saveToPng = awe_renderbuffer_save_to_png

saveToJpeg :: RenderBuffer -> String -> Int -> IO Bool
saveToJpeg = awe_renderbuffer_save_to_jpeg

getAlphaAtPoint :: RenderBuffer -> Int -> Int -> IO Int
getAlphaAtPoint = awe_renderbuffer_get_alpha_at_point

flushAlpha :: RenderBuffer -> IO ()
flushAlpha = awe_renderbuffer_flush_alpha

