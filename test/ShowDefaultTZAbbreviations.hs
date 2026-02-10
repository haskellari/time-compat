module Main where

import Data.Time.Compat

showTZ :: TimeZone -> String
showTZ tz =
  (formatTime defaultTimeLocale "%Z %z" tz)
    ++ ( if timeZoneSummerOnly tz
           then " DST"
           else ""
       )

main :: IO ()
main = mapM_ (\tz -> putStrLn (showTZ tz)) (knownTimeZones defaultTimeLocale)
