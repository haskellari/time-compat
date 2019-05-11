{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Data.Time.Calendar.Compat (
    -- * Days
    Day(..),addDays,diffDays,

    -- * CalendarDiffTime
    CalendarDiffDays (..),
    calendarDay,calendarWeek,calendarMonth,calendarYear,scaleCalendarDiffDays,

    -- * Gregorian calendar
    toGregorian,fromGregorian,fromGregorianValid,showGregorian,gregorianMonthLength,

    -- calendrical arithmetic
    -- e.g. "one month after March 31st"
    addGregorianMonthsClip,addGregorianMonthsRollOver,
    addGregorianYearsClip,addGregorianYearsRollOver,
    addGregorianDurationClip,addGregorianDurationRollOver,
    diffGregorianDurationClip,diffGregorianDurationRollOver,

    -- re-exported from OrdinalDate
    isLeapYear ,

      -- * Week
    DayOfWeek(..), dayOfWeek,
    ) where

import Data.Time.Orphans ()

import Data.Time.Calendar

import Data.Monoid (Monoid (..))
import Data.Data (Data, Typeable)
import Data.Semigroup (Semigroup (..))

-------------------------------------------------------------------------------
-- CalendarDiffTime
-------------------------------------------------------------------------------

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_base(1,9,2)
deriving instance Typeable CalendarDiffDays
deriving instance Data CalendarDiffDays
#endif

#if !MIN_VERSION_time(1,9,0)

data CalendarDiffDays = CalendarDiffDays
    { cdMonths :: Integer
    , cdDays :: Integer
    } deriving (Eq,
    Data
#if __GLASGOW_HASKELL__ >= 802
#endif
    ,Typeable
#if __GLASGOW_HASKELL__ >= 802
#endif
    )

-- | Additive
instance Semigroup CalendarDiffDays where
    CalendarDiffDays m1 d1 <> CalendarDiffDays m2 d2 = CalendarDiffDays (m1 + m2) (d1 + d2)

-- | Additive
instance Monoid CalendarDiffDays where
    mempty  = CalendarDiffDays 0 0
    mappend = (<>)

instance Show CalendarDiffDays where
    show (CalendarDiffDays m d) = "P" ++ show m ++ "M" ++ show d ++ "D"

calendarDay :: CalendarDiffDays
calendarDay = CalendarDiffDays 0 1

calendarWeek :: CalendarDiffDays
calendarWeek = CalendarDiffDays 0 7

calendarMonth :: CalendarDiffDays
calendarMonth = CalendarDiffDays 1 0

calendarYear :: CalendarDiffDays
calendarYear = CalendarDiffDays 12 0

-- | Scale by a factor. Note that @scaleCalendarDiffDays (-1)@ will not perfectly invert a duration, due to variable month lengths.
scaleCalendarDiffDays :: Integer -> CalendarDiffDays -> CalendarDiffDays
scaleCalendarDiffDays k (CalendarDiffDays m d) = CalendarDiffDays (k * m) (k * d)

#endif

-------------------------------------------------------------------------------
-- Gregorian
-------------------------------------------------------------------------------

#if !MIN_VERSION_time(1,9,0)

-- | Add months (clipped to last day), then add days
addGregorianDurationClip :: CalendarDiffDays -> Day -> Day
addGregorianDurationClip (CalendarDiffDays m d) day = addDays d $ addGregorianMonthsClip m day

-- | Add months (rolling over to next month), then add days
addGregorianDurationRollOver :: CalendarDiffDays -> Day -> Day
addGregorianDurationRollOver (CalendarDiffDays m d) day = addDays d $ addGregorianMonthsRollOver m day

-- | Calendrical difference, with as many whole months as possible
diffGregorianDurationClip :: Day -> Day -> CalendarDiffDays
diffGregorianDurationClip day2 day1 = let
    (y1,m1,d1) = toGregorian day1
    (y2,m2,d2) = toGregorian day2
    ym1 = y1 * 12 + toInteger m1
    ym2 = y2 * 12 + toInteger m2
    ymdiff = ym2 - ym1
    ymAllowed =
        if day2 >= day1 then
        if d2 >= d1 then ymdiff else ymdiff - 1
        else if d2 <= d1 then ymdiff else ymdiff + 1
    dayAllowed = addGregorianDurationClip (CalendarDiffDays ymAllowed 0) day1
    in CalendarDiffDays ymAllowed $ diffDays day2 dayAllowed

-- | Calendrical difference, with as many whole months as possible.
-- Same as 'diffGregorianDurationClip' for positive durations.
diffGregorianDurationRollOver :: Day -> Day -> CalendarDiffDays
diffGregorianDurationRollOver day2 day1 = let
    (y1,m1,d1) = toGregorian day1
    (y2,m2,d2) = toGregorian day2
    ym1 = y1 * 12 + toInteger m1
    ym2 = y2 * 12 + toInteger m2
    ymdiff = ym2 - ym1
    ymAllowed =
        if day2 >= day1 then
        if d2 >= d1 then ymdiff else ymdiff - 1
        else if d2 <= d1 then ymdiff else ymdiff + 1
    dayAllowed = addGregorianDurationRollOver (CalendarDiffDays ymAllowed 0) day1
    in CalendarDiffDays ymAllowed $ diffDays day2 dayAllowed

#endif

-------------------------------------------------------------------------------
-- DayOfWeek
-------------------------------------------------------------------------------

#if !MIN_VERSION_time(1,9,0)

data DayOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Show, Read)

-- | \"Circular\", so for example @[Tuesday ..]@ gives an endless sequence.
-- Also: 'fromEnum' gives [1 .. 7] for [Monday .. Sunday], and 'toEnum' performs mod 7 to give a cycle of days.
instance Enum DayOfWeek where
    toEnum i =
        case mod i 7 of
            0 -> Sunday
            1 -> Monday
            2 -> Tuesday
            3 -> Wednesday
            4 -> Thursday
            5 -> Friday
            _ -> Saturday
    fromEnum Monday = 1
    fromEnum Tuesday = 2
    fromEnum Wednesday = 3
    fromEnum Thursday = 4
    fromEnum Friday = 5
    fromEnum Saturday = 6
    fromEnum Sunday = 7
    enumFromTo wd1 wd2
        | wd1 == wd2 = [wd1]
    enumFromTo wd1 wd2 = wd1 : enumFromTo (succ wd1) wd2
    enumFromThenTo wd1 wd2 wd3
        | wd2 == wd3 = [wd1, wd2]
    enumFromThenTo wd1 wd2 wd3 = wd1 : enumFromThenTo wd2 (toEnum $ (2 * fromEnum wd2) - (fromEnum wd1)) wd3

dayOfWeek :: Day -> DayOfWeek
dayOfWeek (ModifiedJulianDay d) = toEnum $ fromInteger $ d + 3

#endif
