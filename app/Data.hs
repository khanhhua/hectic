module Data where

import Text.Printf

data Month = January | February | March | April | May | June | July | August | Septemper | October | November | December
  deriving (Show, Eq, Enum, Ord, Bounded)

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show, Eq, Enum, Ord, Bounded)


data DailyHourMinute = DailyHourMinute Weekday Int Int
  deriving (Eq, Ord)

instance Show DailyHourMinute where
  show (DailyHourMinute w h m) = printf "%s (%02d:%02d)" (show w) h m

instance Bounded DailyHourMinute where
  minBound = DailyHourMinute Sunday 0 0
  maxBound = DailyHourMinute Saturday 23 59

data TimePoint
  = Absolute Int
  | Monthly Month
  | Daily DailyHourMinute

data Schedule = Free | Busy TimePoint TimePoint | Series [Schedule]

instance Semigroup Schedule where
  Free <> Free = Free
  s    <> Free = s
  Busy (Absolute f1) (Absolute t1) <> Busy (Absolute f2) (Absolute t2)
    -- Non-overlapping
    | t1 < f2 = Series [Busy (Absolute f1) (Absolute t1), Busy (Absolute f2) (Absolute t2)]
    | t2 < f1 = Series [Busy (Absolute f2) (Absolute t2), Busy (Absolute f1) (Absolute t1)]
    -- Overlapping
    | otherwise = Busy (Absolute (min f1 f2)) (Absolute (max t1 t2))
  Busy (Absolute f1) (Absolute t1) <> Busy _ _ = error "Invalid argument"
  Busy (Monthly f1) (Monthly t1) <> Busy (Monthly f2) (Monthly t2)
    -- Non-overlapping
    | t1 < f2 = Series [Busy (Monthly f1) (Monthly t1), Busy (Monthly f2) (Monthly t2)]
    | t2 < f1 = Series [Busy (Monthly f2) (Monthly t2), Busy (Monthly f1) (Monthly t1)]
    -- Overlapping
    | otherwise = Busy (Monthly (min f1 f2)) (Monthly (max t1 t2))
  Busy (Monthly f1) (Monthly t1) <> Busy _ _ = error "Invalid argument"
  Busy (Daily f1) (Daily t1) <> Busy (Daily f2) (Daily t2)
    -- Non-overlapping
    | t1 < f2 = Series [Busy (Daily f1) (Daily t1), Busy (Daily f2) (Daily t2)]
    | t2 < f1 = Series [Busy (Daily f2) (Daily t2), Busy (Daily f1) (Daily t1)]
    -- Overlapping
    | otherwise = Busy (Daily (min f1 f2)) (Daily (max t1 t2))
  Busy (Daily f1) (Daily t1) <> Busy _ _ = error "Invalid argument"
  Series xs1 <> Series xs2 = Series (xs1 <> xs2)
  Series xs <> s = foldr (<>) s xs


instance Monoid Schedule where
  mempty = Free
  mappend = (<>)

absolute :: Int -> TimePoint
absolute = Absolute

month :: Month -> TimePoint
month = Monthly

daily :: Weekday -> Int -> Int -> TimePoint
daily weekday h m 
  | h >= 0 && h < 24 && m >= 0 && m < 60 = Daily (DailyHourMinute weekday h m)
  | otherwise = error "Invalid argument"

free :: Schedule
free = Free

absoluteBusy :: Int -> Int -> Schedule
absoluteBusy = undefined
