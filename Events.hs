module Events (SeasonType (..), computeSeason, jdToDate) where

import Data.Angle
import Control.Lens.Operators ((%~))
import Control.Lens.Tuple

{-
-- starts with new
phases =
 [
    "\xd83c\xdf11",
    "\xd83c\xdf12",
    "\xd83c\xdf13",
    "\xd83c\xdf14",
    "\xd83c\xdf15",
    "\xd83c\xdf16",
    "\xd83c\xdf17",
    "\xd83c\xdf18"
 ]

sun = "\x2600\xfe0f";
earth = "\xd83c\xdf0e";

data EclipseType = Solar | Lunar deriving (Show, Eq)

eclipses =
 [
    (2015, 2, 20, Solar), (2015, 8, 28, Lunar),
    (2016, 2, 8, Solar), (2016, 8, 1, Solar),
    (2017, 1, 22, Solar), (2017, 7, 21, Solar),
    (2018, 0, 31, Lunar), (2018, 6, 27, Lunar),
    (2019, 0, 21, Lunar), (2019, 6, 2, Solar), (2019, 11, 26, Solar),
    (2020, 5, 21, Solar), (2020, 11, 14, Solar),
    (2021, 5, 10, Solar), (2021, 11, 4, Solar),
    (2022, 4, 16, Lunar), (2022, 10, 8, Lunar),
    (2023, 3, 20, Solar), (2023, 9, 14, Solar),
    (2024, 3, 8, Solar), (2024, 9, 2, Solar)
 ]

showEclipse Solar m
    | m `elem` [1..3] = earth ++ "\n" ++ (phases !! 4) ++ "\n" ++ sun
    | m `elem` [4..6] = earth ++ (phases !! 2) ++ sun
    | m `elem` [7..9] = sun ++ "\n" ++ head phases ++ "\n" ++ earth
    | otherwise = sun ++ (phases !! 6) ++ earth

showEclipse Lunar m
    | m `elem` [1..3] = (phases !! 4) ++ "\n" ++ earth ++ "\n" ++ sun
    | m `elem` [4..6] = (phases !! 2) ++ earth ++ sun
    | m `elem` [7..9] = sun ++ "\n" ++ earth ++ "\n" ++ head phases
    | otherwise = sun ++ earth ++ (phases !! 6)
-}

data SeasonType = Spring | Summer | Fall | Winter deriving (Show, Eq)

{-
seasons =
 [
    (2015, 2, 20, Spring), (2015, 5, 21, Summer), (2015, 8, 23, Fall), (2015, 11, 22, Winter),
    (2016, 2, 20, Spring), (2016, 5, 20, Summer), (2016, 8, 22, Fall), (2016, 11, 21, Winter),
    (2017, 2, 20, Spring), (2017, 5, 21, Summer), (2017, 8, 22, Fall), (2017, 11, 21, Winter),
    (2018, 2, 20, Spring), (2018, 5, 21, Summer), (2018, 8, 23, Fall), (2018, 11, 21, Winter),
    (2019, 2, 20, Spring), (2019, 5, 21, Summer), (2019, 8, 23, Fall), (2019, 11, 22, Winter),
    (2020, 2, 20, Spring), (2020, 5, 20, Summer), (2020, 8, 22, Fall), (2020, 11, 21, Winter),
    (2021, 2, 20, Spring), (2021, 5, 21, Summer), (2021, 8, 22, Fall), (2021, 11, 21, Winter),
    (2022, 2, 20, Spring), (2022, 5, 21, Summer), (2022, 8, 23, Fall), (2022, 11, 21, Winter),
    (2023, 2, 20, Spring), (2023, 5, 21, Summer), (2023, 8, 23, Fall), (2023, 11, 22, Winter),
    (2024, 2, 20, Spring), (2024, 5, 20, Summer), (2024, 8, 22, Fall), (2024, 11, 21, Winter),
    (2025, 2, 20, Spring), (2025, 5, 21, Summer), (2025, 8, 22, Fall), (2025, 11, 21, Winter),
    (2026, 2, 20, Spring), (2026, 5, 21, Summer), (2026, 8, 23, Fall), (2026, 11, 21, Winter),
    (2027, 2, 20, Spring), (2027, 5, 21, Summer), (2027, 8, 23, Fall), (2027, 11, 22, Winter),
    (2028, 2, 20, Spring), (2028, 5, 20, Summer), (2028, 8, 22, Fall), (2028, 11, 21, Winter),
    (2029, 2, 20, Spring), (2029, 5, 21, Summer), (2029, 8, 22, Fall), (2029, 11, 21, Winter),
    (2030, 2, 20, Spring), (2030, 5, 21, Summer), (2030, 8, 22, Fall), (2030, 11, 21, Winter)
 ]

showSeason Spring = earth ++ "\n" ++ sun
showSeason Summer = earth ++ sun
showSeason Fall = sun ++ "\n" ++ earth
showSeason Winter = sun ++ earth
-}

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

computeSeason :: Integral a => SeasonType -> a -> Double
computeSeason e y = j0 + 0.00001*s / dl
    where y' = (fromIntegral y - 2000) / 1000
          j0 = baseJDE e y'
          t = (j0 - 2451545) / 36525
          w = Degrees $ 35999.373*t - 2.47
          dl = 1 + 0.0334*cosine w + 0.0007*cosine (2*w)
          s = sum $ (\(a, b, c) -> a*cosine (Degrees $ b + c*t)) <$> seasonTerms

jdToDate :: (Num a, RealFrac a, Integral b) => a -> (b, b, a)
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

floor' :: (Num a, RealFrac a) => a -> a
floor' = fromIntegral . floor

{-
zodiac =
 [
    (2, 21, "\x2648\xfe0f"),
    (3, 20, "\x2649\xfe0f"),
    (4, 21, "\x264a\xfe0f"),
    (5, 21, "\x264b\xfe0f"),
    (6, 23, "\x264c\xfe0f"),
    (7, 23, "\x264d\xfe0f"),
    (8, 23, "\x264e\xfe0f"),
    (9, 23, "\x264f\xfe0f"),
    (10, 22, "\x2650\xfe0f"),
    (11, 22, "\x2651\xfe0f"),
    (0, 20, "\x2652\xfe0f"),
    (1, 19, "\x2653\xfe0f")
 ]
-}
