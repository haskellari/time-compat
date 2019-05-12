module Main where

import Control.DeepSeq (force)

import Data.Time.Calendar.Compat
import Data.Time.Clock.System.Compat
import Data.Time.Clock.TAI.Compat
import Data.Time.Compat
import Data.Time.Format.Compat
import Test.HUnit.Base               ((@?=))

main :: IO ()
main = do
    utc <- getCurrentTime

    -- UTCTime
    putStrLn $ formatTime defaultTimeLocale rfc822DateFormat (force utc)

    -- ZonedTime
    zt <- getZonedTime
    putStrLn $ formatTime defaultTimeLocale rfc822DateFormat (force zt)

    -- SystemTime
    st <- getSystemTime
    print $ force st

    -- FormatTime DayOfWeek
    formatTime defaultTimeLocale "%u %w %a %A" Monday @?= "1 1 Mon Monday"

    -- TAI taiNominalDayStart
    show (taiNominalDayStart (ModifiedJulianDay 123)) @?= "1859-03-20 00:00:00 TAI"

_ParseTimeInstances :: [()]
_ParseTimeInstances =
    [ () -- test (undefined :: CalendarDiffTime)
    , test (undefined :: Day)
    , () -- test (undefined :: DiffTime)
    , () -- test (undefined :: NominalDiffTime)
    , test (undefined :: UTCTime)
    , test (undefined :: UniversalTime)
    , () -- test (undefined :: CalendarDiffTime)
    , test (undefined :: TimeZone)
    , test (undefined :: TimeOfDay)
    , test (undefined :: LocalTime)
    , test (undefined :: ZonedTime)
    ]
  where
    test :: ParseTime t => t -> ()
    test _ = ()

_FormatTimeInstances :: [()]
_FormatTimeInstances =
    [ () -- test (undefined :: CalendarDiffTime)
    , test (undefined :: Day)
    , () -- test (undefined :: DiffTime)
    , () -- test (undefined :: NominalDiffTime)
    , test (undefined :: UTCTime)
    , test (undefined :: UniversalTime)
    , () -- test (undefined :: CalendarDiffTime)
    , test (undefined :: TimeZone)
    , test (undefined :: TimeOfDay)
    , test (undefined :: LocalTime)
    , test (undefined :: ZonedTime)
    , test (undefined :: DayOfWeek)
    ]
  where
    test :: FormatTime t => t -> ()
    test _ = ()
