{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Data where

import           Text.Printf

data Month = January | February | March | April | May | June | July | August | Septemper | October | November | December deriving
    ( Bounded
    , Enum
    , Eq
    , Ord
    , Show
    )

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving
    ( Bounded
    , Enum
    , Eq
    , Ord
    , Show
    )


data DailyHourMinute
  = DailyHourMinute Weekday Int Int
  deriving (Eq, Ord)

succ' :: (Eq a, Enum a, Bounded a) => a -> a
succ' value
  | value == maxBound = minBound
  | otherwise = succ value

pred' :: (Eq a, Enum a, Bounded a) => a -> a
pred' value
  | value == minBound = maxBound
  | otherwise = pred value

instance Show DailyHourMinute where
  show (DailyHourMinute w h m) = printf "%s (%02d:%02d)" (show w) h m

instance Bounded DailyHourMinute where
  minBound = DailyHourMinute Sunday 0 0
  maxBound = DailyHourMinute Saturday 23 59

instance Enum DailyHourMinute where
  toEnum int = DailyHourMinute (toEnum w) h m
    where
      (w, r) = int `divMod` (24 * 60)
      (h, m) = r `divMod` 60

  fromEnum (DailyHourMinute w h m) = (fromEnum w * 24 * 60) + h * 60 + m

data TimePoint
  = Monthly Month
  | Daily DailyHourMinute
  deriving (Eq, Show)
-- | Repeated TimePoint

data Schedule
  = Free
  | Open TimePoint TimePoint
  | Busy TimePoint TimePoint
  | Series [Schedule]
  deriving (Eq, Show)

instance Semigroup Schedule where
  Free <> s = s
  s    <> Free = s
  Open (Monthly f1) (Monthly t1) <> Open (Monthly f2) (Monthly t2)
    -- Non-overlapping
    | t1 < f2 = Series [Open (Monthly f1) (Monthly t1), Open (Monthly f2) (Monthly t2)]
    | t2 < f1 = Series [Open (Monthly f2) (Monthly t2), Open (Monthly f1) (Monthly t1)]
    -- Overlapping
    | otherwise = Open (Monthly (min f1 f2)) (Monthly (max t1 t2))
  Open (Monthly _) _ <> Open _ _ = error "Invalid argument"
  Open (Daily f1) (Daily t1) <> Open (Daily f2) (Daily t2)
    -- Non-overlapping
    | t1 < f2 = Series [Open (Daily f1) (Daily t1), Open (Daily f2) (Daily t2)]
    | t2 < f1 = Series [Open (Daily f2) (Daily t2), Open (Daily f1) (Daily t1)]
    -- Overlapping
    | otherwise = Open (Daily (min f1 f2)) (Daily (max t1 t2))
  Open (Daily _) _ <> Open _ _ = error "Invalid argument"
  Busy (Monthly f1) (Monthly t1) <> Busy (Monthly f2) (Monthly t2)
    -- Non-overlapping
    | t1 < f2 = Series [Busy (Monthly f1) (Monthly t1), Busy (Monthly f2) (Monthly t2)]
    | t2 < f1 = Series [Busy (Monthly f2) (Monthly t2), Busy (Monthly f1) (Monthly t1)]
    -- Overlapping
    | otherwise = Busy (Monthly (min f1 f2)) (Monthly (max t1 t2))
  Busy (Monthly _) _ <> Busy _ _ = error "Invalid argument"
  Busy (Daily f1) (Daily t1) <> Busy (Daily f2) (Daily t2)
    -- Non-overlapping
    | t1 < f2 = Series [Busy (Daily f1) (Daily t1), Busy (Daily f2) (Daily t2)]
    | t2 < f1 = Series [Busy (Daily f2) (Daily t2), Busy (Daily f1) (Daily t1)]
    -- Overlapping
    | otherwise = Busy (Daily (min f1 f2)) (Daily (max t1 t2))
  Busy (Daily _) _ <> Busy _ _ = error "Invalid argument"
  Open (Monthly fo) (Monthly to) <> Busy (Monthly fb) (Monthly tb)
    | fo < fb && to > tb = Series [ Open (Monthly fo) (Monthly $ pred' fb)
                                  , Busy (Monthly fb) (Monthly tb)
                                  , Open (Monthly $ succ' tb) (Monthly to)
                                  ]
    | fo == fb && to > tb = Series [ Busy (Monthly fb) (Monthly tb)
                                  , Open (Monthly $ succ' tb) (Monthly to)
                                  ]
    | fo < fb && to == tb = Series [ Open (Monthly fo) (Monthly $ pred' fb)
                                  , Busy (Monthly fb) (Monthly tb)
                                  ]
    | fo == fb && to == tb = Busy (Monthly fb) (Monthly tb)
    | fo == tb = Series [ Busy (Monthly fo) (Monthly tb)
                        , Open (Monthly $ succ' tb) (Monthly to)
                        ]
    | to == fb = Series [ Open (Monthly fo) (Monthly $ pred' fb)
                        , Busy (Monthly fb) (Monthly tb)
                        ]
    -- Non-overlapping
    | to < fb = Series [Open (Monthly fo) (Monthly to), Busy (Monthly fb) (Monthly tb)]
    | tb < fo = Series [Busy (Monthly fb) (Monthly tb), Open (Monthly fo) (Monthly to)]
  Open (Daily fo) (Daily to) <> Busy (Daily fb) (Daily tb)
    | fo < fb && to > tb = Series [ Open (Daily fo) (Daily $ pred' fb)
                                  , Busy (Daily fb) (Daily tb)
                                  , Open (Daily $ succ' tb) (Daily to)
                                  ]
    | fo == fb && to > tb = Series [ Busy (Daily fb) (Daily tb)
                                  , Open (Daily $ succ' tb) (Daily to)
                                  ]
    | fo < fb && to == tb = Series [ Open (Daily fo) (Daily $ pred' fb)
                                  , Busy (Daily fb) (Daily tb)
                                  ]
    | fo == fb && to == tb = Busy (Daily fb) (Daily tb)
    | fo == tb = Series [ Busy (Daily fo) (Daily tb)
                        , Open (Daily $ succ' tb) (Daily to)
                        ]
    | to == fb = Series [ Open (Daily fo) (Daily $ pred' fb)
                        , Busy (Daily fb) (Daily tb)
                        ]
    -- Non-overlapping
    | to < fb = Series [Open (Daily fo) (Daily to), Busy (Daily fb) (Daily tb)]
    | tb < fo = Series [Busy (Daily fb) (Daily tb), Open (Daily fo) (Daily to)]
  
  Busy fb tb <> Open fo to = Open fo to <> Busy fb tb
  Series xs1 <> Series xs2 = Series $ xs1 <> xs2
  Series xs <> s = foldr (<>) s xs
  s <> Series xs = foldr (<>) s xs


instance Monoid Schedule where
  mempty = Free
  mappend = (<>)

monthly :: Month -> TimePoint
monthly = Monthly

weekly :: Weekday -> (Int, Int) -> TimePoint
weekly weekday (h, m)
  | h >= 0 && h < 24 && m >= 0 && m < 60 = Daily (DailyHourMinute weekday h m)
  | otherwise = error "Invalid argument"

free :: Schedule
free = Free

busyMonth :: Month -> Month -> Schedule
busyMonth m1 m2 =
  Busy (monthly m1) (monthly m2)

busyDay :: Weekday -> (Int, Int) -> (Int, Int) -> Schedule
busyDay w (h0, m0) (h1, m1) =
  Busy (weekly w (h0, m0)) (weekly w (h1, m1))

openMonth :: Month -> Month -> Schedule
openMonth m1 m2 =
  Open (monthly m1) (monthly m2)

openDay :: Weekday -> (Int, Int) -> (Int, Int) -> Schedule
openDay w (h0, m0) (h1, m1) =
  Open (weekly w (h0, m0)) (weekly w (h1, m1))
