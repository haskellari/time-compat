module Main (main) where

import Data.Time.Compat

main :: IO ()
main = do
  now <- getZonedTime
  putStrLn $ show now
