\begin{code}
{-# LANGUAGE BangPatterns #-}
import Data.Word (Word8, Word32)
--import Ix
import Control.Parallel.Strategies
import Control.Parallel
import Prelude as P
import Control.Monad.Identity
import Data.Array.Repa as R
import System.Environment
import qualified Graphics.HGL as G

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
convergeIt !n (a, b) (x, y)	= if a*a + b*b > 4 then
																n
															else
																convergeIt (n + 1) (nr, ni) (x, y)
															where 
																nr = a*a - b*b + x
									 							ni = 2*a*b + y

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
		mv = P.map (P.map (toComplex (fromIntegral w, fromIntegral h))) l

mandelPar   :: Word32 -> Word32 -> [[Word8]]
mandelPar = undefined

mandelREPA  :: Word32 -> Word32 -> [[Word8]]
mandelREPA = undefined

drawMandel w h colors = do
	G.runGraphics $ do
		window <- G.openWindow "Mandelbrot" (w, h)
		G.drawInWindow window $ G.overGraphics $
			let
				coords = makeList w h
				mk = P.zipWith (,) (P.concat coords) (P.concat colors)
				f = P.filter (\v -> 255 > snd v ) mk
			in 
				P.map (\(xy, c) -> G.withTextColor (G.RGB c c c) (G.text xy ".")) f

main = do
	drawMandel 640 640 $ mandelStrat 640 640

\end{code}