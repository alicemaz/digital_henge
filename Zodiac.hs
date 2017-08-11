module Zodiac (checkZodiac) where

import Data.List
import Control.Lens.Operators ((^.))
import Control.Lens.Tuple

-- don't need year but rather keep the signatures the same
checkZodiac :: Integer -> Integer -> Integer -> Maybe String
checkZodiac _ m d = (^. _3) <$> find (\(m', d', _) -> m == m' && d == d') zodiac

zodiac :: [(Integer, Integer, String)]
zodiac =
 [
    (3, 21, "\x2648\xfe0f"),
    (4, 20, "\x2649\xfe0f"),
    (5, 21, "\x264a\xfe0f"),
    (6, 21, "\x264b\xfe0f"),
    (7, 23, "\x264c\xfe0f"),
    (8, 23, "\x264d\xfe0f"),
    (9, 23, "\x264e\xfe0f"),
    (10, 23, "\x264f\xfe0f"),
    (11, 22, "\x2650\xfe0f"),
    (12, 22, "\x2651\xfe0f"),
    (1, 20, "\x2652\xfe0f"),
    (2, 19, "\x2653\xfe0f")
 ]
