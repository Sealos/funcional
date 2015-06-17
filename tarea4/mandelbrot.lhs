\begin{code}
{-# LANGUAGE BangPatterns #-}
import Data.Word (Word8, Word32)
import Graphics.UI.SDL as SDL
import Ix

type Complex = (Double, Double)

screenSize :: Complex
screenSize = (1024, 768)

scaleVal :: Double -> Double -> Double
scaleVal a v = (a - v') * 2 / v'
				where
					v' = v / 2

scale :: Complex -> Complex
scale (a, b) = (a', b')
			where
				(x, y) = screenSize
				a' = scaleVal a x
				b' = scaleVal b y

converge :: Complex -> Word8
converge x = convergeIt 0 (0, 0) $ scale x

convergeIt :: Word8 -> Complex -> Complex -> Word8
convergeIt 255 _	_					=	255
convergeIt !n (a, b) (x, y)	=	if a*a + b*b > 4 then
																n
															else
																convergeIt (n + 1) (nr, ni) (x, y)
															where 
																nr = a*a - b*b + x
									 							ni = 2*a*b + y

mandelStrat :: Word32 -> Word32 -> [[Word8]]
mandelStrat = undefined

mandelPar   :: Word32 -> Word32 -> [[Word8]]
mandelPar = undefined

mandelREPA  :: Word32 -> Word32 -> [[Word8]]
mandelREPA = undefined

{-drawMandel f = 
	G.runGraphics $ do
      window <- G.openWindow "Conjunto mandelbrot" screenSize
      G.drawInWindow window $ G.overGraphics (
        let f (Poly c p)   = G.withColor c $ G.polyline (map fix p)
            f (Text c p s) = G.withColor c $ G.text (fix p) s
            (x0,y0)        = origin w h
            fix (x,y)      = (x0 + x, y0 - y)
        in DF.toList $ fmap f (drw (execState (monadicPlot p) initial))
        )
      G.getKey window
      G.closeWindow window-}


createColor screen r g b = SDL.mapRGB (SDL.surfaceGetPixelFormat screen) r g b

main = do
  SDL.init [InitEverything]
  setVideoMode 256 256 32 []
  screen <- getVideoSurface
  drawGrad screen
  SDL.flip screen
  quitHandler

-- Given coordinates, create a 1x1 rectangle containing that pixel
getPix x y = SDL.Rect (fromIntegral x) (fromIntegral y) 1 1

-- Draw a pixel on a surface with color determined by position
drawPixel surf (x, y) = do
    let pixRect = Just (getPix x y)
    pixColor <- createColor surf x y 255
    SDL.fillRect surf pixRect pixColor

-- Apply drawPixel to each coordinate on the screen
drawGrad screen = mapM_ (drawPixel screen) $ range ((0,0), screenSize)

\end{code}