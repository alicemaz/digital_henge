{-# LANGUAGE ImplicitParams #-}

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Approx

import Types
import Season ()
import Zodiac ()
import Moon ()
import Util

-- returns Maybe (Bool, Bool)
-- maybe indicates presence/absence of event
-- first bool correct event, second bool whether within an hour
newMoon = case (checkEvent 2017 9 20 :: EventResult Moon) of
    e@(Event _ t) -> Just (e =~ Event New "", "201709200500" < t && t < "201709200600")
    _ -> Nothing

firstMoon = case (checkEvent 2017 9 28 :: EventResult Moon) of
    e@(Event _ t) -> Just (e =~ Event FirstQuarter "", "201709280223" < t && t < "201709280323")
    _ -> Nothing

fullMoon = case (checkEvent 2017 10 5 :: EventResult Moon) of
    e@(Event _ t) -> Just (e =~ Event Full "", "201710051810" < t && t < "201710051910")
    _ -> Nothing

thirdMoon = case (checkEvent 2017 10 12 :: EventResult Moon) of
    e@(Event _ t) -> Just (e =~ Event ThirdQuarter "", "201710121155" < t && t < "201710121255")
    _ -> Nothing

tests = hUnitTestToTests $ TestList
 [
    "jd to timestring" ~: jdToTimestring 2457984.720370 ~?= "201708190517",
    "date to jd" ~: dateToJD 1957 10 4.81 ~?= 2436116.31,
    "equinox" ~: (checkEvent 2021 3 20 :: EventResult Season) ~?= (Event Spring "202103200937"),
    "no equinox" ~: (checkEvent 2021 3 21 :: EventResult Season) ~?= Nil,
    "zodiac" ~: (checkEvent 2017 3 21 :: EventResult Zodiac) ~?= (Event Aries "201703210000"),
    "no zodiac" ~: (checkEvent 2017 3 20 :: EventResult Zodiac) ~?= Nil,
    "new moon" ~: newMoon ~?= Just (True, True),
    "first quarter" ~: firstMoon ~?= Just (True, True),
    "full moon" ~: fullMoon ~?= Just (True, True),
    "third quarter" ~: thirdMoon ~?= Just (True, True)
    --let ?epsilon = 0.0001 in "loltest" ~: (0.01 :: Double) ~?~ 0.0100001,
 ]

main = defaultMain tests
