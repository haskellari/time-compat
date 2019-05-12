{-# LANGUAGE CPP #-}
module Data.Time.Orphans where

import Data.Orphans ()

import Control.DeepSeq (NFData (..))
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.TAI
import Data.Time.Format

#if !MIN_VERSION_time(1,4,0)
instance NFData Day where
    rnf (ModifiedJulianDay d) = rnf d

instance NFData UniversalTime where
    rnf (ModJulianDate d) = rnf d

instance NFData DiffTime where
    rnf d = d `seq` ()

instance NFData AbsoluteTime where
    rnf d = d `seq` ()

instance NFData UTCTime where
    rnf (UTCTime d t) = rnf d `seq` rnf t

instance NFData NominalDiffTime where
    rnf d = d `seq` ()

instance NFData LocalTime where
    rnf (LocalTime d tod) = rnf d `seq` rnf tod

instance NFData ZonedTime where
    rnf (ZonedTime lt tz) = rnf lt `seq` rnf tz

instance NFData TimeOfDay where
    rnf (TimeOfDay h m s) = rnf h `seq` rnf m `seq` rnf s

instance NFData TimeZone where
    rnf (TimeZone a b c) = rnf a `seq` rnf b `seq` rnf c
#endif

#if !MIN_VERSION_time(1,6,0)
instance ParseTime UniversalTime where
    -- substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    -- parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l xs = localTimeToUT1 0 (buildTime l xs)

instance FormatTime UniversalTime where
    formatCharacter c = fmap (\f tl fo t -> f tl fo (ut1ToLocalTime 0 t)) (formatCharacter c)
#endif
