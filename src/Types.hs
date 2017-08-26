{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import Data.List

-- displays emoji representation of an event
class DisplayEvent a where
    displayEvent :: a -> String

-- given ymd returns event result
class CheckEvent p where
    checkEvent :: Integral a => a -> a -> a -> EventResult p

-- extremely rude
class SemiEq a where
    (=~) :: a -> a -> Bool

-- maybe-alike, either event specifier and timestring, or nil
data EventResult p = forall a. (CheckEvent a, Show a) => Event a String | Nil
deriving instance Show (EventResult p)

-- event specifiers
-- season on its own refers to equinox or solstice
-- in the context of eclipse, the three month period centered on one
data Season = Spring | Summer | Autumn | Winter
    deriving (Show, Eq, Enum)

data Eclipse = Solar Season | Lunar Season
    deriving (Show, Eq)

data Zodiac = Aries | Taurus | Gemini | Cancer | Leo | Virgo |
    Libra | Scorpio | Sagittarius | Capricorn | Aquarius | Pisces
        deriving (Show, Eq, Enum)

data Moon = New | WaxingCrescent | FirstQuarter | WaxingGibbous |
    Full | WaningGibbous | ThirdQuarter | WaningCrescent
        deriving (Show, Eq, Enum)

-- mappings between emoji strings and types
instance DisplayEvent Season where
    displayEvent = genericIndex seasons . fromEnum

instance DisplayEvent Eclipse where
    displayEvent = genericIndex eclipses . fromEnum

instance DisplayEvent Zodiac where
    displayEvent = genericIndex zodiacSigns . fromEnum

instance DisplayEvent Moon where
    displayEvent = genericIndex moonPhases . fromEnum

-- enum instance, since deriving an enum that contains an enum isn't supported
-- I don't like using magic numbers but doubt the number of seasons will change anytime soon
instance Enum Eclipse where
    fromEnum (Solar a) = fromEnum a
    fromEnum (Lunar a) = fromEnum a + 4
    toEnum 0 = Solar Spring
    toEnum 1 = Solar Summer
    toEnum 2 = Solar Autumn
    toEnum 3 = Solar Winter
    toEnum 4 = Lunar Spring
    toEnum 5 = Lunar Summer
    toEnum 6 = Lunar Autumn
    toEnum 7 = Lunar Winter
    toEnum _ = undefined

-- forgive me for my sins
instance Eq (EventResult p) where
    (==) = heq where
        heq :: EventResult a -> EventResult b -> Bool
        heq (Event i x) (Event j y) = show i == show j && x == y
        heq Nil Nil = True
        heq _ _ = False

instance SemiEq (EventResult p) where
    (=~) = heq where
        heq :: EventResult a -> EventResult b -> Bool
        heq (Event i _) (Event j _) = show i == show j
        heq Nil Nil = True
        heq _ _ = False

seasons :: [String]
seasons =
 [
    earth ++ "\n" ++ sun,
    earth ++ sun,
    sun ++ "\n" ++ earth,
    sun ++ earth
 ]

eclipses :: [String]
eclipses =
 [
    earth ++ "\n" ++ (moonPhases !! 4) ++ "\n" ++ sun,
    earth ++ (moonPhases !! 2) ++ sun,
    sun ++ "\n" ++ head moonPhases ++ "\n" ++ earth,
    sun ++ (moonPhases !! 6) ++ earth,
    (moonPhases !! 4) ++ "\n" ++ earth ++ "\n" ++ sun,
    (moonPhases !! 2) ++ earth ++ sun,
    sun ++ "\n" ++ earth ++ "\n" ++ head moonPhases,
    sun ++ earth ++ (moonPhases !! 6)
 ]

zodiacSigns :: [String]
zodiacSigns =
 [
    "\x2648\xfe0f",
    "\x2649\xfe0f",
    "\x264a\xfe0f",
    "\x264b\xfe0f",
    "\x264c\xfe0f",
    "\x264d\xfe0f",
    "\x264e\xfe0f",
    "\x264f\xfe0f",
    "\x2650\xfe0f",
    "\x2651\xfe0f",
    "\x2652\xfe0f",
    "\x2653\xfe0f"
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
