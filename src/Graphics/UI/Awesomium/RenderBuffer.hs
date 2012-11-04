----------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Awesomium.RenderBuffer
-- Copyright   :  (c) 2012 Maksymilian Owsianny
-- License     :  LGPL-3 (see the file LICENSE)
-- 
-- Maintainer  :  Maksymilian.Owsianny+Awesomium@gmail.com
-- Stability   :  Experimental
-- Portability :  Portable? (needs FFI)
--
----------------------------------------------------------------------

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

-- | Get the width (in pixels) of a 'RenderBuffer'.
getWidth :: RenderBuffer -> IO Int
getWidth = awe_renderbuffer_get_width

-- | Get the height (in pixels) of a 'RenderBuffer'.
getHeight :: RenderBuffer -> IO Int
getHeight = awe_renderbuffer_get_height

-- | Get the rowspan (number of bytes per row) of a 'RenderBuffer'.
getRowspan :: RenderBuffer -> IO Int
getRowspan = awe_renderbuffer_get_rowspan

-- | Get a pointer to the actual pixel buffer within a 'RenderBuffer'.
getBuffer :: RenderBuffer -> IO (Ptr CUChar)
getBuffer = awe_renderbuffer_get_buffer

-- Copy a 'RenderBuffer' to a specific destination with the same
-- dimensions.
-- copyTo :: RenderBuffer -> (unsigned char* dest_buffer) -> Int -> Int -> Bool -> Bool -> IO ()
-- copyTo = awe_renderbuffer_copy_to

-- Copy a 'RenderBuffer' to a pixel buffer with a floating-point pixel
-- format for use with game engines like Unity3D.
-- copyToFloat :: RenderBuffer -> (float* dest_buffer) -> IO ()
-- copyToFloat = awe_renderbuffer_copy_to_float

-- | Save a copy of this 'RenderBuffer' to a PNG image file.
saveToPng :: RenderBuffer -> String -> Bool -> IO Bool
saveToPng = awe_renderbuffer_save_to_png

-- | Save a copy of this 'RenderBuffer' to a JPEG image file with
-- quality 1 to 100.
saveToJpeg :: RenderBuffer -> String -> Int -> IO Bool
saveToJpeg = awe_renderbuffer_save_to_jpeg

-- | Get the alpha value at a certain point (origin is top-left). This
-- is useful for alpha-picking.
getAlphaAtPoint
    :: RenderBuffer
    -> Int -- ^ The x-value of the point.
    -> Int -- ^ The y-value of the point.
    -> IO Int -- ^ Returns the alpha value at a certain point (255 is
    -- comppletely opaque, 0 is completely transparent).
getAlphaAtPoint = awe_renderbuffer_get_alpha_at_point

-- | Sets the alpha channel to completely opaque values.
flushAlpha :: RenderBuffer -> IO ()
flushAlpha = awe_renderbuffer_flush_alpha
