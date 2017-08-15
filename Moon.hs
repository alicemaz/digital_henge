module Moon where --(moonPos, sunPos, moonIlum) where

import Data.Tuple.Curry
import Control.Lens.Operators
import Control.Lens.Tuple

import Util

-- args order is asc decl dist of moon, asc decl dist of sun
moonIlum :: RealFloat a => a -> a -> a -> a -> a -> a -> a
moonIlum a d del a0 d0 r = (1 + cos' i) / 2
    where p = acos' $ sin' d0*sin' d + cos' d0*cos' d*cos' (a0 - a)
          i = atan2' (r*sin' p) (del - r*cos' p)

-- this is only accurate to a half minute of arc or so
-- since it disregards gravitational effects outside sun/earth system
-- l' and m same as below
-- e' is earth's eccentricity
-- c is equation of center
-- resulting in longitude in degrees and distance in au
-- converts to apparent right ascension and declination, and distance to km
-- since the sun's latitude is never more than a second or two of arc, we take it at 0 for simplicity
--sunPos :: Double -> (Double, Double, Double)
sunPos :: RealFloat a => a -> (a, a, a)
sunPos jde = (atan2' (cos' e*sin' l) (cos' l), asin' $ sin' e*sin' l, 149598000*r)
    where t = (jde - 2451545) / 36525
          l' = sunMeanLong t
          m = solarAnomaly t
          e' = 0.016708617 - 0.000042037*t - 0.0000001236*t^2
          c = (1.914600 - 0.004817*t - 0.000014*t^2)*sin' m
              + (0.019993 - 0.000101*t)*sin' (2*m)
              + 0.00029*sin' (3*m)
          r = 1.000001018*(1 - e'^2) / (1 + e'*cos' (m+c))
          o = 125.04 - 1934.136*t
          l = l' + c - 0.00569 - 0.00478*sin' o
          e = (obliqNutation t ^. _1) + 0.00256*cos' o

-- l' is mean longitude
-- d is mean elongation
-- m is sun's mean anomoly
-- m' is moon's mean anomoly
-- f is moon's mean distance from ascending node
-- and each m must be scaled to take into account eccentricity of earth's orbit
-- further corrections are made to the sums to account for action of venus and jupiter
-- produces geometric latitude and longitude in degrees and distance in km
-- again converted from ecliptic and returned as apparent equitoral asc/decl
moonPos :: Double -> (Double, Double, Double)
moonPos jde = (asc, decl, dist)
    where t = (jde - 2451545) / 36525
          l' = moonMeanLong t
          d = 297.8502042 + 445267.1115168*t - 0.00163*t^2 + t^3/545868 - t^4/113065000
          m = solarAnomaly t
          m' = 134.9634114 + 477198.8676313*t + 0.0089970*t^2 + t^3/69699 - t^4/14712000
          f = 93.2720993 + 483202.0175273*t - 0.0034029*t^2 -t^3/3526000 + t^4/863310000
          a1 = 119.75 + 131.849*t
          a2 = 53.09 + 479264.290*t
          a3 = 313.45 + 481266.484*t
          ls = (uncurryN $ computeTerm sin' t d m m' f) <$> longTermCoeffs
          l = sum ls + 3958*sin' a1 + 1962*sin' (l' - f) + 318*sin' a2
          bs = (uncurryN $ computeTerm sin' t d m m' f) <$> latTermCoeffs
          b = sum bs - 2235*sin' l' + 382*sin' a3 + 175*sin' (a1 - f) + 175*sin' (a1 + f)
              + 127*sin' (l' - m') - 115*sin' (l' + m')
          r = sum $ (uncurryN $ computeTerm cos' t d m m' f) <$> distTermCoeffs
          lat = b / 1000000
          long = l' + l/1000000
          dist = 385000.56 + r/1000
          (e, dP) = obliqNutation t
          appLong = simplA' $ long + dP
          (asc, decl) = ascDecl lat appLong e

-- right ascension and declination, function of lat long and true obliquity
ascDecl :: RealFloat a => a -> a -> a -> (a, a)
ascDecl b l e = (a, d)
    where a = atan2' (sin' l*cos' e - tan' b*sin' e) (cos' l)
          d = asin' $ sin' b*cos' e + cos' b*sin' e*sin' l

-- true ecliptic obliquity (little epsilon) and nutation in longitude (delta psi)
obliqNutation :: Floating a => a -> (a, a)
obliqNutation t = (e0 + dE, dP)
    where u = t/100
          e0 = 23.43929 - 0.01300417*u - 1.55*u^2 + 1999.25*u^3 - 51.38*u^4 - 249.67*u^5
               - 39.05*u^6 + 7.12*u^7 + 27.87*u^8 + 5.79*u^9 + 2.45*u^10
          o = 125.04452 - 1934.136261*t
          lm = moonMeanLong t
          ls = sunMeanLong t
          dE = 0.002555556*cos' o + 0.0001583333*cos' (2*ls) + 0.00002777778*cos' (2*lm) - 0.000025*cos' (2*o)
          dP = -0.004777778*cos' o - 0.0003666667*sin' (2*ls) - 0.0000638889*sin (2*lm) - 0.00005833333*cos' (2*o)

sunMeanLong :: Fractional a => a -> a
sunMeanLong t = 280.46645 + 36000.76983*t + 0.0003032*t^2

moonMeanLong :: Fractional a => a -> a
moonMeanLong t = 218.3164591 + 481267.88134236*t - 0.0013268*t^2 + t^3/538841 - t^4/65194000

solarAnomaly :: Fractional a => a -> a
solarAnomaly t = 357.5291092 + 35999.0502909*t - 0.0001536*t^2 + t^3/24490000

-- computes a term of the three sums giving moon's geometric lat/long and distance
computeTerm :: (Fractional a, Eq a) => (a -> a) -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a
computeTerm w t d m m' f cd cm cm' cf c
    | abs cm == 1 = e * s
    | abs cm == 2 = e^2 * s
    | otherwise = s
        where e = 1 - 0.002516*t - 0.0000074*t^2
              s = c * w (cd*d + cm*m + cm'*m' + cf*f)

longTermCoeffs :: [(Double, Double, Double, Double, Double)]
distTermCoeffs :: [(Double, Double, Double, Double, Double)]
(longTermCoeffs, distTermCoeffs) = (g longCoeffs, g distCoeffs)
    where g = foldr f [] . zip longDistTerms
          f ((w, x, y, z), Just c) acc = (w, x, y, z, c) : acc
          f (_, Nothing) acc = acc

latTermCoeffs :: [(Double, Double, Double, Double, Double)]
latTermCoeffs =
 [
    (0, 0, 0, 1, 5128122), (0, 0, 1, 1, 280602), (0, 0, 1, -1, 277693), (2, 0, 0, -1, 173237), (2, 0, -1, 1, 55413),
    (2, 0, -1, -1, 46271), (2, 0, 0, 1, 32573), (0, 0, 2, 1, 17198), (2, 0, 1, -1, 9266), (0, 0, 2, -1, 8822),
    (2, -1, 0, -1, 8216), (2, 0, -2, -1, 4324), (2, 0, 1, 1, 4200), (2, 1, 0, -1, -3359), (2, -1, -1, 1, 2463),
    (2, -1, 0, 1, 2211), (2, -1, -1, -1, 2065), (0, 1, -1, -1, -1870), (4, 0, -1, -1, 1828), (0, 1, 0, 1, -1794),
    (0, 0, 0, 3, -1749), (0, 1, -1, 1, -1565), (1, 0, 0, 1, -1491), (0, 1, 1, 1, -1475), (0, 1, 1, -1, -1410),
    (0, 1, 0, -1, -1344), (1, 0, 0, -1, -1335), (0, 0, 3, 1, 1107), (4, 0, 0, -1, 1021), (4, 0, -1, 1, 833),
    (0, 0, 1, -3, 777), (4, 0, -2, 1, 671), (2, 0, 0, -3, 607), (2, 0, 2, -1, 596), (2, -1, 1, -1, 491),
    (2, 0, -2, 1, -451), (0, 0, 3, -1, 439), (2, 0, 2, 1, 422), (2, 0, -3, -1, 421), (2, 1, -1, 1, -366),
    (2, 1, 0, 1, -351), (4, 0, 0, 1, 331), (2, -1, 1, 1, 315), (2, -2, 0, -1, 302), (0, 0, 1, 3, -283),
    (2, 1, 1, -1, -229), (1, 1, 0, -1, 223), (1, 1, 0, 1, 223), (0, 1, -2, -1, -220), (2, 1, -1, -1, -220),
    (1, 0, 1, 1, -185), (2, -1, -2, -1, 181), (0, 1, 2, 1, -177), (4, 0, -2, -1, 176), (4, -1, -1, -1, 166),
    (1, 0, 1, -1, -164), (4, 0, 1, -1, 132), (1, 0, -1, -1, -119), (4, -1, 0, -1, 115), (2, -2, 0, 1, 107)
 ]

longDistTerms :: [(Double, Double, Double, Double)]
longDistTerms =
 [
    (0, 0, 1, 0), (2, 0, -1, 0), (2, 0, 0, 0), (0, 0, 2, 0), (0, 1, 0, 0),
    (0, 0, 0, 2), (2, 0, -2, 0), (2, -1, -1, 0), (2, 0, 1, 0), (2, -1, 0, 0),
    (0, 1, -1, 0), (1, 0, 0, 0), (0, 1, 1, 0), (2, 0, 0, -2), (0, 0, 1, 2),
    (0, 0, 1, -2), (4, 0, -1, 0), (0, 0, 3, 0), (4, 0, -2, 0), (2, 1, -1, 0),
    (2, 1, 0, 0), (1, 0, -1, 0), (1, 1, 0, 0), (2, -1, 1, 0), (2, 0, 2, 0),
    (4, 0, 0, 0), (2, 0, -3, 0), (0, 1, -2, 0), (2, 0, -1, 2), (2, -1, -2, 0),
    (1, 0, 1, 0), (2, -2, 0, 0), (0, 1, 2, 0), (0, 2, 0, 0), (2, -2, -1, 0),
    (2, 0, 1, -2), (2, 0, 0, 2), (4, -1, -1, 0), (0, 0, 2, 2), (3, 0, -1, 0),
    (2, 1, 1, 0), (4, -1, -2, 0), (0, 2, -1, 0), (2, 2, -1, 0), (2, 1, -2, 0),
    (2, -1, 0, -2), (4, 0, 1, 0), (0, 0, 4, 0), (4, -1, 0, 0), (1, 0, -2, 0),
    (2, 1, 0, -2), (0, 0, 2, -2), (1, 1, 1, 0), (3, 0, -2, 0), (4, 0, -3, 0),
    (2, -1, 2, 0), (0, 2, 1, 0), (1, 1, -1, 0), (2, 0, 3, 0), (2, 0, -1, -2)
 ]

longCoeffs :: [Maybe Double]
longCoeffs = Just <$>
 [
    6288774, 1274027, 658314, 213618, -185116, -114332, 58793, 57066, 53322, 45758,
    -40923, -34720, -30383, 15327, -12528, 10980, 10675, 10034, 8548, -7888,
    -6766, -5163, 4987, 4036, 3994, 3861, 3665, -2689, -2602, 2390,
    -2348, 2236, -2120, -2069, 2048, -1773, -1595, 1215, -1110, -892,
    -810, 759, -713, -700, 691, 596, 549, 537, 520, -487,
    -399, -381, 351, -340, 330, 327, -323, 299, 294
 ]

distCoeffs :: [Maybe Double]
distCoeffs =
 [
    Just (-20905355), Just (-3699111), Just (-2955968), Just (-569925), Just 48888,
    Just (-3149), Just 246158, Just (-152138), Just (-170733), Just (-204586),
    Just (-129620), Just 108743, Just 104755, Just 10321, Nothing,
    Just 79661, Just (-34782), Just (-23210), Just (-21636), Just 24208,
    Just 30824, Just (-8379), Just (-16675), Just (-12831), Just (-10445),
    Just (-11650), Just 14403, Just (-7003), Nothing, Just 10056,
    Just 6322, Just (-9884), Just 5751, Nothing, Just (-4950),
    Just 4130, Nothing, Just (-3958), Nothing, Just 3258,
    Just 2616, Just (-1897), Just (-2117), Just 2354, Nothing,
    Nothing, Just (-1423), Just (-1117), Just (-1571), Just (-1739),
    Nothing, Just (-4421), Nothing, Nothing, Nothing,
    Nothing, Just 1165, Nothing, Nothing, Just 8752
 ]
