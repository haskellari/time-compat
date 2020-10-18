{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}
#endif
module Data.Time.Calendar.Quarter.Compat (
    QuarterOfYear(..), addQuarters, diffQuarters,
    Quarter(..),
#if __GLASGOW_HASKELL__ >= 710
    pattern YearQuarter,
#endif
    monthOfYearQuarter,
    monthQuarter,
    dayQuarter
) where

#if MIN_VERSION_time(1,11,0)
import Data.Time.Calendar.Quarter
#else

import Data.Data                       (Data)
import Data.Typeable                   (Typeable)
import Text.Read                       (Read (..))
import Data.Fixed                      (mod', divMod')
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP    (char)

import Data.Time.Calendar
import Data.Time.Calendar.Types
import Data.Time.Calendar.Private
import Data.Time.Calendar.Month.Compat

-- | Quarters of each year. Each quarter corresponds to three months.
data QuarterOfYear = Q1 | Q2 | Q3 | Q4 deriving (Eq, Ord, Data, Typeable, Read, Show)

-- | maps Q1..Q4 to 1..4
instance Enum QuarterOfYear where
    toEnum i =
        case mod' i 4 of
            1 -> Q1
            2 -> Q2
            3 -> Q3
            _ -> Q4
    fromEnum Q1 = 1
    fromEnum Q2 = 2
    fromEnum Q3 = 3
    fromEnum Q4 = 4

instance Bounded QuarterOfYear where
    minBound = Q1
    maxBound = Q4

-- | An absolute count of year quarters.
-- Number is equal to @(year * 4) + (quarterOfYear - 1)@.
newtype Quarter = MkQuarter Integer deriving (Eq, Ord, Data, Typeable)

-- | Show as @yyyy-Qn@.
instance Show Quarter where
    show q = case fromYearQuarter q of
      (y, qy) -> show4 y ++ "-" ++ show qy

-- | Read as @yyyy-Qn@.
instance Read Quarter where
    readPrec = do
        y <- readPrec
        _ <- lift $ char '-'
        m <- readPrec
        return $ toYearQuarter y m

addQuarters :: Integer -> Quarter -> Quarter
addQuarters n (MkQuarter a) = MkQuarter $ a + n

diffQuarters :: Quarter -> Quarter -> Integer
diffQuarters (MkQuarter a) (MkQuarter b) = a - b

#if __GLASGOW_HASKELL__ >= 710
-- | Bidirectional abstract constructor.
pattern YearQuarter :: Year -> QuarterOfYear -> Quarter
pattern YearQuarter y qy <- (fromYearQuarter -> (y, qy))
  where YearQuarter y qy = toYearQuarter y qy

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE YearQuarter #-}
#endif
#endif

monthOfYearQuarter :: MonthOfYear -> QuarterOfYear
monthOfYearQuarter my | my <= 3 = Q1
monthOfYearQuarter my | my <= 6 = Q2
monthOfYearQuarter my | my <= 9 = Q3
monthOfYearQuarter _ = Q4

monthQuarter :: Month -> Quarter
monthQuarter m = case fromYearMonth m of
    (y, my) -> toYearQuarter y $ monthOfYearQuarter my

dayQuarter :: Day -> Quarter
dayQuarter d = case toMonthDay d of
    (m, _) -> monthQuarter m

-- | Part of @YearQuarter@ pattern
toYearQuarter :: Year -> QuarterOfYear -> Quarter
toYearQuarter y qy = MkQuarter $ y * 4 + toInteger (pred $ fromEnum qy)

-- | Part of @YearQuarter@ pattern
fromYearQuarter :: Quarter -> (Year, QuarterOfYear)
fromYearQuarter (MkQuarter y) = case divMod' y 4 of
    (y, qy) -> (y, toEnum (succ (fromInteger qy)))


-- | Part of 'MonthDay' pattern
toMonthDay :: Day -> (Month,DayOfMonth)
toMonthDay d = case toGregorian d of 
    (y, my, dm) -> (toYearMonth y my, dm)

-- | Part of @YearMonth@ pattern
toYearMonth :: Year -> MonthOfYear -> Month
toYearMonth y my = MkMonth $ (y * 12) + toInteger (pred $ clip 1 12 my)

-- | Part of @YearMonth@ pattern
fromYearMonth :: Month -> (Year, MonthOfYear)
fromYearMonth (MkMonth m) = case divMod' m 12 of
    (y, my) -> (y, succ (fromInteger my))

#endif
