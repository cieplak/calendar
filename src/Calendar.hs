{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calendar where

class DayCount a where
  dayCount :: a -> Integer

class Format a where
  format :: a -> String

class Indexed a where
  index :: a -> Integer

newtype Year = Year Integer deriving (Eq, Ord, Show)

newtype Week = Week Integer

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Eq, Ord, Show)

instance Indexed Month where
  index January   =  0
  index February  =  1
  index March     =  2
  index April     =  3
  index May       =  4
  index June      =  5
  index July      =  6
  index August    =  7
  index September =  8
  index October   =  9
  index November  = 10
  index December  = 11

instance Num Month where
  a + b = fromInteger (((index a) + (index b)) `mod` 12)
  a - b = fromInteger (((index a) - (index b)) `mod` 12)
  fromInteger  0 = January
  fromInteger  1 = February
  fromInteger  2 = March
  fromInteger  3 = April
  fromInteger  4 = May
  fromInteger  5 = June
  fromInteger  6 = July
  fromInteger  7 = August
  fromInteger  8 = September
  fromInteger  9 = October
  fromInteger 10 = November
  fromInteger 11 = December
  fromInteger i = fromInteger (i `mod` 12)

data Weekday
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Ord, Show)

instance Indexed Weekday where
  index Sunday    = 0
  index Monday    = 1
  index Tuesday   = 2
  index Wednesday = 3
  index Thursday  = 4
  index Friday    = 5
  index Saturday  = 6

instance Num Weekday where
  a + b = fromInteger (((index a) + (index b)) `mod` 7)
  a - b = fromInteger (((index a) - (index b)) `mod` 7)
  fromInteger 0 = Sunday
  fromInteger 1 = Monday
  fromInteger 2 = Tuesday
  fromInteger 3 = Wednesday
  fromInteger 4 = Thursday
  fromInteger 5 = Friday
  fromInteger 6 = Saturday
  fromInteger i = fromInteger (i `mod` 7)

data Date
  = Date
  { dateYear      :: Year
  , dateMonth     :: Month
  , dateDay       :: Integer
  , dateDayOfWeek :: Weekday
  } deriving (Eq, Ord, Show)

isLeapYear :: Year -> Bool
isLeapYear (Year n)
  = case (n `mod` 4) /= 0 of
      True -> False                        -- not divisble by 4          -> common year
      False -> case (n `mod` 100) /= 0 of
        True -> True                       -- divisible by 4 but not 100 -> leap year
        False -> case (n `mod` 400) /= 0 of
          True -> False                    -- divisible by 4 but not 400 -> common year
          False -> True                    -- divisble by 4 and 400      -> leap year

instance DayCount Week where
  dayCount week = 7

type MonthYear = (Month, Year)

instance DayCount MonthYear where
  dayCount (January  , year) = 31
  dayCount (February , year) = case isLeapYear year of
                                 False -> 28
                                 True  -> 29
  dayCount (March    , year) = 31
  dayCount (April    , year) = 30
  dayCount (May      , year) = 31
  dayCount (June     , year) = 30
  dayCount (July     , year) = 31
  dayCount (August   , year) = 31
  dayCount (September, year) = 30
  dayCount (October  , year) = 31
  dayCount (November , year) = 30
  dayCount (December , year) = 31

instance DayCount Year where
  dayCount year
    = dayCount (January  , year)
    + dayCount (February , year)
    + dayCount (March    , year)
    + dayCount (April    , year)
    + dayCount (May      , year)
    + dayCount (June     , year)
    + dayCount (July     , year)
    + dayCount (August   , year)
    + dayCount (September, year)
    + dayCount (October  , year)
    + dayCount (November , year)
    + dayCount (December , year)

date :: Year
     -> Month
     -> Integer  -- e.g. 1-31
     -> Date

-- Base Case
date (Year 1753) (January) (1) = Date (Year 1753) January 1 Monday

-- Recursive Definition
date year month day
  = Date year month day dayOfWeek
    where
      dayOfWeek = dateDayOfWeek yesterday + 1
      yesterday = date y m d
      (y, m, d) = (previousDay year month day)

previousDay :: Year -> Month -> Integer -> (Year, Month, Integer)
previousDay (Year year) month day
  = case (day == 1) of
      False -> (Year year, month, day')
        where day' = day - 1
      True -> case (month == 0) of
        False -> (Year year, month', day')
          where
            month' = month - 1
            day'   = dayCount (month', Year year)
        True -> (Year year', month', day')
          where
            year'  = year - 1
            month' = month - 1
            day'   = dayCount (month', Year year')
