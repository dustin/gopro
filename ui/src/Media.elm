module Media exposing (..)

import Iso8601
import Time
import Json.Decode as Decode exposing (Decoder, int, string, float)
import Json.Decode.Pipeline exposing (required, optional)

type alias Medium =
    { id : String
    , camera_model : String
    , captured_at : Time.Posix
    , created_at : Time.Posix
    , file_size : Int
    , moments_count : Int
    , ready_to_view : String
    , resolution : String
    , source_duration : String
    , media_type : String
    , token : String
    , width : Int
    , height : Int
    }

mediaDecoder : Decoder Medium
mediaDecoder =
    Decode.succeed Medium
        |> required "id" string
        |> optional "camera_model" string "Unknown"
        |> required "captured_at" Iso8601.decoder
        |> required "created_at" Iso8601.decoder
        |> required "file_size" int
        |> required "moments_count" int
        |> required "ready_to_view" string
        |> optional "resolution" string ""
        |> optional "source_duration" string ""
        |> optional "type" string "Unknown"
        |> required "token" string
        |> required "width" int
        |> required "height" int

mediaListDecoder : Decoder (List Medium)
mediaListDecoder = Decode.list mediaDecoder
