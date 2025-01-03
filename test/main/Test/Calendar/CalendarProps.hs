module Test.Calendar.CalendarProps (
    testCalendarProps,
) where

import Data.Time.Calendar.Month.Compat
import Data.Time.Calendar.Quarter.Compat
import Test.Arbitrary ()
import Test.Tasty
import Test.TestUtil

testYearMonth :: TestTree
testYearMonth = nameTest "YearMonth" $ \m -> case m of
    YearMonth y my -> m == YearMonth y my

testMonthDay :: TestTree
testMonthDay = nameTest "MonthDay" $ \d -> case d of
    MonthDay m dm -> d == MonthDay m dm

testYearQuarter :: TestTree
testYearQuarter = nameTest "YearQuarter" $ \q -> case q of
    YearQuarter y qy -> q == YearQuarter y qy

testCalendarProps :: TestTree
testCalendarProps = nameTest "calender-props" [testYearMonth, testMonthDay, testYearQuarter]
