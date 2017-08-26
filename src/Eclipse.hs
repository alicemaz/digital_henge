module Eclipse where

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
