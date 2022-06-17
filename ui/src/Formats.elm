module Formats exposing (comma, millis, time, day, bytes, httpErr, float)

import Time
import List.Extra exposing (groupWhile, unfoldr)
import Filesize
import Http
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)

comma : Int -> String
comma i = let seg x = String.padLeft (if x >= 1000 then 3 else 0) '0' (String.fromInt (modBy 1000 x))
              parts = List.Extra.unfoldr (\x -> if x == 0
                                                then Nothing
                                                else Just (seg x, x // 1000)) i
          in String.join "," (List.reverse parts)

millis : Int -> String
millis i = let seg x = String.padLeft (if x >= 60 then 2 else 0) '0' (String.fromInt (modBy 60 x))
               parts = List.Extra.unfoldr (\x -> if x == 0
                                                 then Nothing
                                                 else Just (seg x, x // 60)) (i // 1000)
           in String.join ":" (List.reverse parts)

time : Time.Zone -> Time.Posix -> String
time z t = String.fromInt (Time.toHour z t) ++ ":" ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute z t))

monthStr : Time.Month -> String
monthStr month =
  case month of
    Time.Jan -> "01"
    Time.Feb -> "02"
    Time.Mar -> "03"
    Time.Apr -> "04"
    Time.May -> "05"
    Time.Jun -> "06"
    Time.Jul -> "07"
    Time.Aug -> "08"
    Time.Sep -> "09"
    Time.Oct -> "10"
    Time.Nov -> "11"
    Time.Dec -> "12"

day : Time.Zone -> Time.Posix -> String
day z t = let two x = String.padLeft 2 '0' (String.fromInt x) in
          String.fromInt (Time.toYear z t) ++ "-" ++ monthStr (Time.toMonth z t) ++ "-" ++ two (Time.toDay z t)

httpErr : Http.Error -> String
httpErr e = case e of
                Http.BadUrl u -> "bad url: " ++ u
                Http.Timeout -> "timeout"
                Http.NetworkError -> "network error"
                Http.BadStatus i -> "bad status: " ++ String.fromInt i
                Http.BadBody b -> "bad body: " ++ b

bytes : Int -> String
bytes n = let cfg = Filesize.defaultSettings
              decies = if n > 1000000000000 then 3
                       else if n < 1000000000 then 0
                       else 2
          in
              Filesize.formatWith {cfg | decimalPlaces=decies} n

float : Float -> String
float = format usLocale
