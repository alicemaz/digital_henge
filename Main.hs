module Main where

import Data.List
import Data.Maybe
import System.Environment
import System.Console.GetOpt

import Season
import Zodiac
import Moon

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

simpl a
    | a < 0 = a + n
    | a > 360 = a - n
    | otherwise = a
        where n = fromIntegral $ 360 * (abs $ floor $ a / 360)

main :: IO ()
main = do
    --argv <- getArgs
    --runmode <- processArgs argv
    putStrLn $ "season: " ++ show (checkSeason 2021 3 20)
    putStrLn $ "zodiac: " ++ show (checkZodiac 2017 3 21)
    let (x, y, z) = moonPos 2448724.5
    putStrLn $ "longitude: " ++ show (simpl x)
    putStrLn $ "latitude: " ++ show y
    putStrLn $ "distance: " ++ show z
