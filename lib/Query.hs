module Query where

import Data
import Data.Time (UTCTime)

data Query = Query

data TimeSlot = TimeSlot UTCTime UTCTime

query :: TimePoint -> Query
query t = Query

runQuery :: Query -> Schedule -> [TimeSlot]
runQuery q schedule = undefined