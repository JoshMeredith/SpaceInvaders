{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

-- Functions that are imported from JS
module WasmImports where

#if WASM_BUILD
import Foreign.Ptr ( Ptr )
import Foreign.C ( CChar, newCStringLen )
import Foreign (free)
#elif JS_BUILD
import GHC.JS.Prim
#endif

foreign import ccall "clearCanvas" clearCanvas :: Int -> Int -> Int -> IO ()
foreign import ccall "fillStyle" fillStyle :: Int -> Int -> Int -> IO ()
foreign import ccall "strokeStyle" strokeStyle :: Int -> Int -> Int -> IO ()
foreign import ccall "fillRect" fillRect :: Double -> Double -> Double -> Double -> IO ()
foreign import ccall "getCanvasWidth" getCanvasWidth :: IO Int
foreign import ccall "getCanvasHeight" getCanvasHeight :: IO Int
-- void fillText(char* textPtr, int textLen, double x, double y, double maxWidth) {
#if WASM_BUILD
foreign import ccall "fillText" fillText :: Ptr CChar -> Int -> Double -> Double -> Double -> IO ()
foreign import ccall "setFont" setFont :: Ptr CChar -> Int -> IO ()
#elif JS_BUILD
foreign import javascript "fillText" fillText :: JSVal -> Double -> Double -> Double -> IO ()
foreign import javascript "setFont" setFont :: JSVal -> IO ()
#endif
-- void arc(double x, double y, double radius, double startAngle, double endAngle, bool counterclockwise);
foreign import ccall "arc" arc :: Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
-- void ellipse(double x, double y, double radiusX, double radiusY, double rotation, double startAngle, double endAngle, bool counterclockwise);
foreign import ccall "ellipse" ellipse :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
foreign import ccall "fill" fill :: IO ()
foreign import ccall "beginPath" beginPath :: IO ()
foreign import ccall "closePath" closePath :: IO ()
foreign import ccall "stroke" stroke :: IO ()
foreign import ccall "moveTo" moveTo :: Double -> Double -> IO ()
foreign import ccall "lineTo" lineTo :: Double -> Double -> IO ()
foreign import ccall "setLineWidth" setLineWidth :: Double -> IO ()


-- Helper function to avoid dealing with manual memory management
#if WASM_BUILD
fillTextHelper :: String -> Double -> Double -> Double -> IO ()
fillTextHelper textStr x y maxWidth = do
  (buf, len) <- newCStringLen textStr
  fillText buf len x y maxWidth
  free buf

setFontHelper :: String -> IO ()
setFontHelper textStr = do
  (buf, len) <- newCStringLen textStr
  setFont buf len
  free buf
#elif JS_BUILD
fillTextHelper :: String -> Double -> Double -> Double -> IO ()
fillTextHelper textStr x y maxWidth = 
  fillText (toJSString textStr) x y maxWidth

setFontHelper :: String -> IO ()
setFontHelper = setFont . toJSString
#endif

renderCircle :: Double -> Double -> Double -> Int -> Int -> Int -> IO ()
renderCircle posX posY radius colR colG colB = do
  beginPath
  arc posX posY radius 0 (2*pi) False
  fillStyle colR colG colB
  fill