{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Data.Time.Clock.TAI.Compat (
    -- * TAI arithmetic
    AbsoluteTime,taiEpoch,addAbsoluteTime,diffAbsoluteTime,
    taiNominalDayStart,

    -- * leap-second map type
    LeapSecondMap',

    -- * conversion between UTC and TAI with map
    utcDayLength,utcToTAITime,taiToUTCTime,

    taiClock,
    ) where

import Data.Time.Orphans ()

import Data.Time.Compat
import Data.Time.Clock.TAI

-- | This type is either 'LeapSecondMap' or 'LeapSecondTable', depending
-- on the version of @time@ (changed in @time-1.7.0@).
#if MIN_VERSION_time(1,7,0)
type LeapSecondMap' = LeapSecondMap
#else
type LeapSecondMap' = LeapSecondTable
#endif

#if !(MIN_VERSION_time(1,8,0))
taiNominalDayStart :: Day -> AbsoluteTime
taiNominalDayStart (ModifiedJulianDay ds) =
   addAbsoluteTime (secondsToDiffTime (ds * 86400)) taiEpoch

-- | TAI clock, if it exists. Note that it is unlikely to be set correctly, without due care and attention.
taiClock :: Maybe (DiffTime,IO AbsoluteTime)
taiClock = Nothing
#endif
