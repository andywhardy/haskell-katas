{-|
  Module         : BerlinClock.hs
  Description    : An implementation of http://agilekatas.co.uk/katas/BerlinClock-Kata
-}

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Fixed

type Lamp = Bool
type LampLine = [Lamp]

data BerlinClock = BerlinClock { secondsLine :: Lamp,
                                 fiveHourLine :: LampLine,
                                 hourLine ::  LampLine,
                                 fiveMinuteLine ::  LampLine,
                                 minuteLine :: LampLine } 

instance Show BerlinClock where
  show (BerlinClock secondsLine fiveHourLine hourLine fiveMinuteLine minuteLine) = "BerlinClock( \n" ++ 
                                                                        "secondsLine=" ++ (show secondsLine) ++ "\n" ++
                                                                        "fiveHourLine=" ++ (show fiveHourLine) ++ "\n" ++
                                                                        "hourLine=" ++ (show hourLine) ++ "\n" ++
                                                                        "fiveMinuteLine=" ++ (show fiveMinuteLine) ++ "\n" ++
                                                                        "minuteLine=" ++ (show minuteLine) ++ ")\n"

line numberOn lineLength  = onLine ++ offLine
  where onLine = replicate numberOn True
        numberOff = lineLength - numberOn
        offLine = (replicate numberOff False)

main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
        numberFiveHours = div hour 5
        numberHours = mod hour 5
        numberFiveMinutes = div minute 5
        numberMinutes = mod minute 5
        secondsLine = (even . floor) second 
        fiveHourLine = (line numberFiveHours 4)
        hourLine = (line numberHours 4)
        fiveMinuteLine = (line numberFiveMinutes 11)
        minuteLine =  (line numberMinutes 4)
        result = BerlinClock secondsLine fiveHourLine hourLine fiveMinuteLine minuteLine
    putStrLn $ "Converting " ++ show now ++ " into: "
    putStrLn $ show result
