module Zodiac () where

import Data.List

import Types
import Util

instance CheckEvent Zodiac where
    checkEvent y m d = case findIndex (== (m, d)) zodiacDates of
        Just i -> Event (toEnum i :: Zodiac) (dateToTimestring y' m' d')
            where (y', m', d') = (toInteger y, toInteger m, fromRational (toRational d))
        Nothing -> Nil

zodiacDates :: Integral a => [(a, a)]
zodiacDates =
 [
    (3, 21), (4, 20), (5, 21), (6, 21), (7, 23), (8, 23),
    (9, 23), (10, 23), (11, 22), (12, 22), (1, 20), (2, 19)
 ]
