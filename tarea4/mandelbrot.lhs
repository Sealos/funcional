\begin{code}
{-# LANGUAGE BangPatterns #-}
import Data.Word (Word8, Word32)
import Ix
import Control.Parallel.Strategies
import Control.Parallel
import Prelude
import Control.Monad.Identity
import Data.Array.Repa
import System.Environment

type Complex = (Double, Double)

scaleVal :: Double -> Double -> Double
scaleVal a v = (a - v') * 2 / v'
				where
					v' = v / 2

scale :: Complex -> Complex -> Complex
scale f (a, b) = (a', b')
			where
				(x, y) = f
				a' = scaleVal a x
				b' = scaleVal b y

converge :: Complex -> Word8
converge x = convergeIt 0 (0, 0) x

convergeIt :: Word8 -> Complex -> Complex -> Word8
convergeIt 255 _	_					=	255
convergeIt !n (a, b) (x, y)	=	if a*a + b*b > 4 then
																n
															else
																convergeIt (n + 1) (nr, ni) (x, y)
															where 
																nr = a*a - b*b + x
									 							ni = 2*a*b + y

makeList :: Word32 -> Word32 -> [[(Word32, Word32)]]
makeList a b = foldl (\l e -> (zip (repeat e) ys):l) [] xs
	where
		xs = [0..a]
		ys = [0..b]

toComplex :: Complex -> (Word32, Word32) -> Complex
toComplex c (x, y) = scale c (f x, f y)
	where
		f = fromIntegral

mandelStrat :: Word32 -> Word32 -> [[Word8]]
mandelStrat w h = parMap rpar (parMap rdeepseq converge) mv
	where
		l = makeList w h
		mv = map (map (toComplex (fromIntegral w, fromIntegral h))) l

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

{-
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
-}
\end{code}