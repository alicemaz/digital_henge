module Moon (moonPos) where

import Data.Tuple.Curry

import Util

-- so l' is mean longitude
-- d is mean elongation
-- m is sun's mean anomoly
-- m' is moon's mean anomoly
-- f is moon's mean distance from ascending node
-- and m must be scaled by e to take into account eccentricity of earth's orbit
-- further corrections are made to the sums to account for action of venus and jupiter
-- returns longitude and latitude in degrees and distance in km
moonPos :: Double -> (Double, Double, Double)
moonPos jde = (l' + l/1000000, b/1000000, 385000.56 + r/1000)
    where t = (jde - 2451545) / 36525
          l' = 218.3164591 + 481267.88134236*t - 0.0013268*t^2 + t^3/538841 - t^4/65194000
          d = 297.8502042 + 445267.1115168*t - 0.00163*t^2 + t^3/545868 - t^4/113065000
          m = 357.5291092 + 35999.0502909*t - 0.0001536*t^2 + t^3/24490000
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

computeTerm w t d m m' f cd cm cm' cf c
    | abs cm == 1 = e * s
    | abs cm == 2 = e^2 * s
    | otherwise = s
        where e = 1 - 0.002516*t - 0.0000074*t^2
              s = c * w (cd*d + cm*m + cm'*m' + cf*f)

(longTermCoeffs, distTermCoeffs) = (g longCoeffs, g distCoeffs)
    where g = foldr f [] . zip longDistTerms
          f ((w, x, y, z), Just c) acc = (w, x, y, z, c) : acc
          f (_, Nothing) acc = acc

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

--longDistTerms :: [(Double, Double, Double, Double)]
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
longCoeffs =
 [
    Just 6288774, Just 1274027, Just 658314, Just 213618, Just (-185116),
    Just (-114332), Just 58793, Just 57066, Just 53322, Just 45758,
    Just (-40923), Just (-34720), Just (-30383), Just 15327, Just (-12528),
    Just 10980, Just 10675, Just 10034, Just 8548, Just (-7888),
    Just (-6766), Just (-5163), Just 4987, Just 4036, Just 3994,
    Just 3861, Just 3665, Just (-2689), Just (-2602), Just 2390,
    Just (-2348), Just 2236, Just (-2120), Just (-2069), Just 2048,
    Just (-1773), Just (-1595), Just 1215, Just (-1110), Just (-892),
    Just (-810), Just 759, Just (-713), Just (-700), Just 691,
    Just 596, Just 549, Just 537, Just 520, Just (-487),
    Just (-399), Just (-381), Just 351, Just (-340), Just 330,
    Just 327, Just (-323), Just 299, Just 294, Nothing
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
