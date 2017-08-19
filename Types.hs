module Types where

import Data.List

-- associates emoji represntations with displayable events
-- namely moon phase, equinox/solstice, eclipse, and zodiac
class PrettyShow a where
    prettyShow :: a -> String

data SeasonType = Spring | Summer | Fall | Winter deriving (Show, Eq, Enum)

instance PrettyShow SeasonType where
    prettyShow = genericIndex seasons . fromEnum

seasons :: [String]
seasons =
 [
    "spr", -- earth ++ "\n" ++ sun,
    "sum", -- earth ++ sun,
    "fal", -- sun ++ "\n" ++ earth,
    "wint" -- sun ++ earth
 ]

moonPhases :: [String]
moonPhases =
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

sun :: String
sun = "\x2600\xfe0f";

earth :: String
earth = "\xd83c\xdf0e";
