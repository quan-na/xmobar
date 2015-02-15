module Plugins.TextClock (TextClock(..)) where

import Plugins
import Data.Time.LocalTime

readNum :: Int -> String
readTen :: Int -> String
readHour :: Int -> String
readMinute :: Int -> String
readTime :: TimeOfDay -> String

readHour h = readNum(rem h 12)
readMinute m = readNum m
readTime t = (readHour (todHour t)) ++ " " ++ (readMinute (todMin t))

readNum 0 = "zerO"
readNum 1 = "oNe"
readNum 2 = "Two"
readNum 3 = "thRee"
readNum 4 = "fouR"
readNum 5 = "Five"
readNum 6 = "sIx"
readNum 7 = "seveN"
readNum 8 = "Eight"
readNum 9 = "ninE"
readNum 10 = "Ten"
readNum 11 = "eleVen"
readNum 12 = "tweLve"
readNum 13 = "thirtEen"
readNum 14 = "fouRteen"
readNum 15 = "fifteeN"
readNum 16 = "Sixteen"
readNum 17 = "seveNteen"
readNum 18 = "eIghteen"
readNum 19 = "nineTeen"
readNum n | n > 19 = "<fc=white>" ++ (readTen (quot n 10)) ++ "</fc>"
                     ++ (if 0==(rem n 10) then ""
                        else " " ++ (readNum (rem n 10)))

readTen 2 = "tweNty"
readTen 3 = "tHirty"
readTen 4 = "foRty"
readTen 5 = "fifTy"
readTen 6 = "siXty"

data TextClock = TextClock String Int
    deriving (Read, Show)

instance Exec TextClock where
    alias (TextClock a _) = a
    rate (TextClock _ r) = r
    run (TextClock _ _) = do
        zt <- getZonedTime
        return (readTime (localTimeOfDay (zonedTimeToLocalTime zt)))

