module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Approx

import Types
import Season
import Zodiac
import Util

tests = hUnitTestToTests $ TestList
 [
    "jd to timestring" ~: jdToTimestring 2457984.720370 ~?= "201708190517",
    "date to jd" ~: dateToJD 1957 10 4.81 ~?= 2436116.31,
    "equinox" ~: (checkEvent 2021 3 20 :: EventResult Season) ~?= (Event Spring "202103200937"),
    "no equinox" ~: (checkEvent 2021 3 21 :: EventResult Season) ~?= Nil,
    "zodiac" ~: (checkEvent 2017 3 21 :: EventResult Zodiac) ~?= (Event Aries "201703210000"),
    "no zodiac" ~: (checkEvent 2017 3 20 :: EventResult Zodiac) ~?= Nil
 ]

main = defaultMain tests
