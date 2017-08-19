module Season (checkSeason) where

import Control.Lens.Operators
import Control.Lens.Tuple

import Types
import Util

-- this is a little odd in that ymd are passed in with an integer day
-- and d' formed from the output of computeSeason is a double
-- so it pulls double (lol) duty in being used for both verifying the day and getting the time
checkSeason :: Integral a => a -> a -> a -> Maybe (String, SeasonType)
checkSeason y m d 
    | m > 0 && m <= 12 && m `mod` 3 == 0 && floor d' == d = Just (jdToTimestring jde, s)
    | otherwise = Nothing
        where s = toEnum $ fromIntegral $ m `div` 3 - 1
              jde = computeSeason s y - deltaT y
              (_, _, d') = jdToDate jde

seasonTerms :: [(Double, Double, Double)]
seasonTerms =
 [
    (485, 324.96, 1934.136), (203, 337.23, 32964.467), (199, 342.08, 20.186),
    (182, 27.85, 445267.112), (156, 73.14, 45036.886), (136, 171.52, 22518.443),
    (77, 222.54, 65928.934), (74, 296.72, 3034.906), (70, 243.58, 9037.513),
    (58, 119.81, 33718.147), (52, 297.17, 150.678), (50, 21.02, 2281.226),
    (45, 247.54, 29929.562), (44, 325.15, 31555.956), (29, 60.93, 4443.417),
    (18, 155.12, 67555.328), (17, 288.79, 4562.452), (16, 198.04, 62894.029),
    (14, 199.76, 31436.921), (12, 95.39, 14577.848), (12, 287.11, 31931.756),
    (12, 320.81, 34777.259), (9, 227.73, 1222.114), (8, 15.45, 16859.074)
 ]

baseJDE :: SeasonType -> Double -> Double
baseJDE Spring y = 2451623.80984 + 365242.37404*y + 0.05169*y^2 - 0.00411*y^3 - 0.00057*y^4
baseJDE Summer y = 2451716.56767 + 365241.62603*y + 0.00325*y^2 + 0.00888*y^3 - 0.00030*y^4
baseJDE Fall y   = 2451810.21715 + 365242.01767*y - 0.11575*y^2 + 0.00337*y^3 + 0.00078*y^4
baseJDE Winter y = 2451900.05952 + 365242.74049*y - 0.06223*y^2 - 0.00823*y^3 + 0.00032*y^4

-- note this returns ephemeris time
computeSeason :: Integral a => SeasonType -> a -> Double
computeSeason e y = j0 + 0.00001*s / dl
    where y' = (fromIntegral y - 2000) / 1000
          j0 = baseJDE e y'
          t = (j0 - 2451545) / 36525
          w = 35999.373*t - 2.47
          dl = 1 + 0.0334*cos' w + 0.0007*cos' (2*w)
          s = sum $ (\(a, b, c) -> a*cos' (b + c*t)) <$> seasonTerms
