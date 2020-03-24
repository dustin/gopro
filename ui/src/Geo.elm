module Geo exposing (..)

import Json.Decode as Decode exposing (Decoder, index, int, field, string, float)

type alias Point =
    { lat : Float
    , lon : Float
    }

pointDecoder : Decoder Point
pointDecoder =
    Decode.map2 Point
        (index 0 float)
        (index 1 float)

type alias Area =
    { name : String
    , nw : Point
    , se : Point
    }

areaDecoder : Decoder Area
areaDecoder =
    Decode.map3 Area
        (field "name" string)
        (field "nw" pointDecoder)
        (field "se" pointDecoder)

inArea : Point -> Area -> Bool
inArea p a = p.lat <= a.nw.lat && p.lat >= a.se.lat && p.lon >= a.nw.lon && p.lon <= a.se.lon
