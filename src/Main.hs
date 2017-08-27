module Main where

import Data.List
import Data.Maybe
import Data.Time
import Control.Lens.Operators
import Control.Lens.Tuple
import System.IO
import System.Process
import System.Environment
import System.Console.GetOpt

import Types
import Season ()
import Zodiac ()
import Moon ()
import Tweet

data Flag = Mode ModeType | Print | TZ deriving (Show, Eq)
data ModeType = Queue | Output OutputType deriving (Show, Eq)
data OutputType = Moon Int | Eclipse Int | Season Int | Zodiac Int deriving (Show, Eq)
type RunMode = (ModeType, Bool, Bool)

isMode :: Flag -> Bool
isMode (Mode _) = True
isMode _ = False

mkMode :: (Int -> OutputType) -> String -> Flag
mkMode e = Mode . Output . e . read

header :: String
header = "usage: henge [OPTIONS]"

options :: [OptDescr Flag]
options =
 [
    Option ['q'] [] (NoArg (Mode Queue)) "queue events for next day",
    Option ['s'] [] (ReqArg (mkMode Season) "INT") "output solstice/equinox",
    Option ['e'] [] (ReqArg (mkMode Eclipse) "INT") "output eclipse",
    Option ['z'] [] (ReqArg (mkMode Zodiac) "INT") "output zodiac sign",
    Option ['m'] [] (ReqArg (mkMode Moon) "INT") "output moon phase",
    Option ['p'] [] (NoArg Print) "don't tweet, print to stdout",
    Option ['t'] [] (NoArg TZ) "ignore incorrect system timezone"
 ]

processArgs :: [String] -> IO RunMode
processArgs argv = case getOpt Permute options argv of
    (o, _, []) -> if (length . filter isMode) o == 1
                  then pure (m, p, t)
                  else ioError $ userError $ "must specify one and only one mode\n" ++ usageInfo header options
                      where Just (Mode m) = find isMode o
                            p = isJust $ find (== Print) o
                            t = isJust $ find (== TZ) o
    (_, _, e) -> ioError $ userError $ concat e ++ usageInfo header options

queue :: IO ()
queue = do
    (y, m0, d0) <- (toGregorian . addDays 1 . utctDay) <$> getCurrentTime
    let (m, d) = (fromIntegral m0, fromIntegral (d0 + 1))
        events = catMaybes
         [
            reifyEvent (checkEvent y m d :: EventResult Season),
            -- XXX TODO FIXME ECLIPSE
            reifyEvent (checkEvent y m d :: EventResult Zodiac),
            reifyEvent (checkEvent y m d :: EventResult Moon)
         ]
    scheduleEvents events

scheduleEvents :: [(String, String, String)] -> IO ()
scheduleEvents [] = pure ()
scheduleEvents ((f, i, t):es) = do
    (Just hin, _, _, _) <- createProcess (proc "at" ["-t", t]) { std_in = CreatePipe }
    hPutStr hin $ "henge " ++ f ++ " " ++ i
    hClose hin
    scheduleEvents es

getOutput :: OutputType -> String
getOutput (Season n) = displayEvent (toEnum n :: Season)
getOutput (Eclipse n) = displayEvent (toEnum n :: Eclipse)
getOutput (Zodiac n) = displayEvent (toEnum n :: Zodiac)
getOutput (Moon n) = displayEvent (toEnum n :: Moon)

main :: IO ()
main = do
    runmode <- processArgs =<< getArgs

    -- I could fix this by injecting an option into toTimestring calls
    -- but idc I'm only running it on one machine
    tz <- getCurrentTimeZone
    if not (runmode ^. _3) && tz /= utc
    then ioError $ userError "by authorial fiat, system timezone must be UTC"
    else pure ()

    case runmode of
        (Queue, _, _) -> queue
        (Output m, False, _) -> tweet $ getOutput m
        (Output m, True, _) -> putStrLn $ show $ getOutput m
