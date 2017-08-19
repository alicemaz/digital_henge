module Main where

import Data.List
import Data.Maybe
import System.IO
import System.Process
import System.Environment
import System.Console.GetOpt

import Season
import Zodiac
import Moon
import Util

data Flag = Mode ModeType | Print deriving (Show, Eq)
data ModeType = Moon | Eclipse | Season | Zodiac deriving (Show, Eq)
type RunMode = (ModeType, Bool)

isMode :: Flag -> Bool
isMode (Mode _) = True
isMode _ = False

header :: String
header = "usage: henge [OPTIONS]"

options :: [OptDescr Flag]
options =
 [
    Option ['m'] ["moon"] (NoArg (Mode Moon)) "moon phase",
    Option ['e'] ["eclipse"] (NoArg (Mode Eclipse)) "eclipse",
    Option ['s'] ["season"] (NoArg (Mode Season)) "solstices/equinoxes",
    Option ['z'] ["zodiac"] (NoArg (Mode Zodiac)) "zodiac sign",
    Option ['p'] ["print"] (NoArg Print) "don't tweet, print to stdout"
 ]

processArgs :: [String] -> IO RunMode
processArgs argv = case getOpt Permute options argv of
    (o, _, []) -> if (length . filter isMode) o == 1
                  then pure (m, p)
                  else ioError $ userError $ "must specify one and only one mode\n" ++ usageInfo header options
                      where Just (Mode m) = find isMode o
                            p = isJust $ find (== Print) o
    (_, _, e) -> ioError $ userError $ concat e ++ usageInfo header options

-- TODO XXX sun pos and moon illum obv
-- but also I need to calc delta t and translate between jd and jde
-- uhh so my two cases so far are... moonPos starts from JDE and returns coords
-- eventually I used those for illuminated fraction
-- but the input will eventually be ymd so I need to convert that to JD and then add dT
-- as for season, input is ymd... but I determine season by month and just compare day
-- so time doesn't actually matter, for correctness tho compute returns JDE so I want to subtract dT
-- in other words I should just use year, I have it onhand in both cases
-- ok so for moon I need longitudes of sun and moon, latitude of moon, both distances
main :: IO ()
main = do
    --argv <- getArgs
    --runmode <- processArgs argv
    (Just hin, _, _, _) <- createProcess (proc "at" ["-t", "201708160007"]) { std_in = CreatePipe }
    hPutStr hin "touch /tmp/lol3\n"
    hClose hin

{-
    let (y, m, d) = (1957, 10, 4.81)
    putStrLn $ "sputnik jd: " ++ show (dateToJD y m d)
    putStrLn $ "season: " ++ show (checkSeason 2021 3 20)
    putStrLn $ "zodiac: " ++ show (checkZodiac 2017 3 21)
    putStrLn $ "deltaT: " ++ show (deltaT 2050)
    let jde = dateToJD 1992 4 12
        (ascM, declM, distM) = moonPos jde
        (ascS, declS, distS) = sunPos jde
        --(cp, p, ti, i, k) = moonIlum ascM declM distM longS distS
    putStrLn $ "jde: " ++ show jde
    putStrLn $ "ascM: " ++ show ascM
    putStrLn $ "declM: " ++ show declM
    putStrLn $ "distM: " ++ show distM
    putStrLn $ "formtest: " ++ show (ascDecl (-3.229127) 133.167269 23.440636) -- lat/long/eps from moon chapter
    putStrLn $ "formtest2: " ++ show (ascDecl 6.684170 113.215630 23.4392911) -- from ex 12.a
    putStrLn $ "illumtest: " ++ show (moonIlum 134.6885 13.7684 368408 20.6579 8.6964 149971520)
    putStrLn $ "illum: " ++ show (moonIlum ascM declM distM ascS declS distS)
    putStrLn $ "cos p: " ++ show cp
    putStrLn $ "p: " ++ show p
    putStrLn $ "tan i: " ++ show ti
    putStrLn $ "i: " ++ show i
    putStrLn $ "k: " ++ show k
-}
