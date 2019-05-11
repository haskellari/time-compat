{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Data.Time.Clock.TAI.Compat (
    -- * TAI arithmetic
    AbsoluteTime,taiEpoch,addAbsoluteTime,diffAbsoluteTime,
    taiNominalDayStart,

    -- * leap-second map type
    LeapSecondMap,

    -- * conversion between UTC and TAI with map
    utcDayLength,utcToTAITime,taiToUTCTime,

    taiClock,
    ) where

import Data.Time.Orphans ()

#if MIN_VERSION_time(1,7,0)
import Data.Time.Clock.TAI
#else

import Control.DeepSeq (NFData (..))
import Data.Data (Data, Typeable)
import Data.Maybe (fromJust)
import Data.Fixed (div')

import Data.Time.Compat

-- | AbsoluteTime is TAI, time as measured by a clock.
newtype AbsoluteTime = MkAbsoluteTime DiffTime deriving (Eq,Ord,Data,Typeable)

instance NFData AbsoluteTime where
    rnf (MkAbsoluteTime a) = rnf a

-- | The epoch of TAI, which is 1858-11-17 00:00:00 TAI.
taiEpoch :: AbsoluteTime
taiEpoch = MkAbsoluteTime 0

taiNominalDayStart :: Day -> AbsoluteTime
taiNominalDayStart day = MkAbsoluteTime $ realToFrac $ (toModifiedJulianDay day) * 86400

-- | addAbsoluteTime a b = a + b
addAbsoluteTime :: DiffTime -> AbsoluteTime -> AbsoluteTime
addAbsoluteTime t (MkAbsoluteTime a) = MkAbsoluteTime (a + t)

-- | diffAbsoluteTime a b = a - b
diffAbsoluteTime :: AbsoluteTime -> AbsoluteTime -> DiffTime
diffAbsoluteTime (MkAbsoluteTime a) (MkAbsoluteTime b) = a - b

instance Show AbsoluteTime where
    show t = show (utcToLocalTime utc (fromJust (taiToUTCTime (const (Just 0)) t))) ++ " TAI" -- ugly, but standard apparently

-- | TAI - UTC during this day.
-- No table is provided, as any program compiled with it would become
-- out of date in six months.
type LeapSecondMap = Day -> Maybe Int

utcDayLength :: LeapSecondMap -> Day -> Maybe DiffTime
utcDayLength lsmap day = do
    i0 <- lsmap day
    i1 <- lsmap $ addDays 1 day
    return $ realToFrac (86400 + i1 - i0)

dayStart :: LeapSecondMap -> Day -> Maybe AbsoluteTime
dayStart lsmap day = do
    i <- lsmap day
    return $ addAbsoluteTime (realToFrac $ (toModifiedJulianDay day) * 86400 + toInteger i) taiEpoch

utcToTAITime :: LeapSecondMap -> UTCTime -> Maybe AbsoluteTime
utcToTAITime lsmap (UTCTime day dtime) = do
    t <- dayStart lsmap day
    return $ addAbsoluteTime dtime t

taiToUTCTime :: LeapSecondMap -> AbsoluteTime -> Maybe UTCTime
taiToUTCTime lsmap abstime = let
    stable day = do
        dayt <- dayStart lsmap day
        len <- utcDayLength lsmap day
        let
            dtime = diffAbsoluteTime abstime dayt
            day' = addDays (div' dtime len) day
        if day == day' then return (UTCTime day dtime) else stable day'
    in stable $ ModifiedJulianDay $ div' (diffAbsoluteTime abstime taiEpoch) 86400

-- | TAI clock, if it exists. Note that it is unlikely to be set correctly, without due care and attention.
taiClock :: Maybe (DiffTime,IO AbsoluteTime)
taiClock = Nothing
#endif

