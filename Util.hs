module Util (rad, deg, sin', cos', tan', asin', acos', atan', atan2', simplA', floor', jdToDate, dateToJD, deltaT) where

import Data.List
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Lens.Traversal

rad :: Floating a => a -> a
rad x = x / 180 * pi

deg :: Floating a => a -> a
deg x = x / pi * 180

sin' :: Floating a => a -> a
cos' :: Floating a => a -> a
tan' :: Floating a => a -> a

sin' = sin . rad
cos' = cos . rad
tan' = tan . rad

asin' :: Floating a => a -> a
acos' :: Floating a => a -> a
atan' :: Floating a => a -> a
atan2' :: RealFloat a => a -> a -> a

asin' = deg . asin
acos' = deg . acos
atan' = deg . atan
atan2' n d = deg $ atan2 n d

simplA' :: RealFrac a => a -> a
simplA' a
    | a < 0 = a + n
    | a > 360 = a - n
    | otherwise = a
        where n = fromIntegral $ 360 * (abs $ floor $ a / 360)

floor' :: RealFrac a => a -> a
floor' = fromIntegral . floor

jdToDate :: (RealFrac a, Integral b) => a -> (b, b, a)
jdToDate j = (year, month, day)
    where (z, f) = _1 %~ fromIntegral $ properFraction $ j + 0.5
          t = floor' $ (z - 1867216.25) / 36524.25
          a = if z < 2299161 then z else z + 1 + t - floor' (t / 4)
          b = a + 1524
          c = floor' $ (b - 122.1) / 365.25
          d = floor' $ 365.25*c
          e = floor' $ (b - d) / 30.6001
          day = b - d - floor' (30.6001*e) + f
          month = floor $ if e < 14 then e - 1 else e - 13
          year = floor $ if month > 2 then c - 4716 else c - 4715

dateToJD :: (RealFrac a, Integral b) => b -> b -> a -> a
dateToJD y m d = floor' (365.25*(y'+4716)) + floor' (30.6001*(m'+1)) + d + b - 1524.5
    where (y', m') = (both %~ fromIntegral) $ if m > 2 then (y, m) else (y-1, m+12)
          a = floor' (y'/100)
          b = 2 - a + floor' (a/4)

-- delta T is emperically determined, fluctuates unpredictably
-- so short of copying historical values, the best way is to approximate small intervals
-- "general" polynomials covering multiple centuries often wrong by several dozen seconds
-- not gonna bother implementing beyond the range I need
-- full list at https://eclipse.gsfc.nasa.gov/LEcat5/deltatpoly.html
-- remember JDE = JD + dT == JD = JDE - dT
deltaTParams :: (Ord a, Fractional a) => [(a -> Bool, a -> a, a -> a)]
deltaTParams =
 [
    ((<1986), undefined, undefined),
    (
        \y -> y >= 1986 && y < 2005,
        \t -> 63.86 + 0.3345*t - 0.060374*t^2 + 0.0017275*t^3 + 0.000651814*t^4 + 0.00002373599*t^5,
        \y -> y - 2000
    ),
    (
        \y -> y >= 2005 && y < 2050,
        \t -> 62.92 + 0.32217*t + 0.005589*t^2,
        \y -> y - 2000
    ),
    (
        \y -> y >= 2050 && y < 2150,
        \y -> -20 + 32*((y-1820)/100)^2 - 0.5628*(2150-y),
        id
    ),
    ((>=2150), undefined, undefined)
 ]

deltaT :: (Real a, Fractional b, Ord b) => a -> b
deltaT y = (g . f) y'
    where y' = fromRational $ toRational y
          Just (_, g, f) = find (\s -> (s ^. _1) y') deltaTParams
