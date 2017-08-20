module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Approx

import Util

tests = hUnitTestToTests $ TestList
 [
    "jd to timestring" ~: jdToTimestring 2457984.720370 ~?= "201708190517",
    "date to jd" ~: dateToJD 1957 10 4.81 ~?= 2436116.31
 ]

main = defaultMain tests
