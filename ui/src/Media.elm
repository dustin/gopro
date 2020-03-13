module Media exposing (MediaType(..), Medium, mediaTypeStr, mediaDecoder, mediaListDecoder)

import Iso8601
import Time
import Json.Decode as Decode exposing (Decoder, int, string, nullable, float)
import Json.Decode.Pipeline exposing (required, optional)

type MediaType = Burst | Photo | TimeLapse | TimeLapseVideo | Video | Unknown

mediaTypeStr t = case t of
                     Burst -> "Burst"
                     Photo -> "Photo"
                     TimeLapse -> "TimeLapse"
                     TimeLapseVideo -> "TimeLapseVideo"
                     Video -> "Video"
                     Unknown -> "Unknown"

strMediaType s = case s of
                     "Burst" -> Burst
                     "Photo" -> Photo
                     "TimeLapse" -> TimeLapse
                     "TimeLapseVideo" -> TimeLapseVideo
                     "Video" -> Video
                     _ -> Unknown

type alias Medium =
    { id : String
    , camera_model : String
    , captured_at : Time.Posix
    , created_at : Time.Posix
    , file_size : Int
    , moments_count : Int
    , ready_to_view : String
    , resolution : String
    , source_duration : Maybe Int
    , media_type : MediaType
    , token : String
    , width : Int
    , height : Int
    }

stringInt : Decoder (Maybe Int)
stringInt = Decode.oneOf [Decode.map String.toInt string, Decode.maybe int]

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
        |> required "source_duration" stringInt
        |> required "type" (Decode.map strMediaType string)
        |> required "token" string
        |> required "width" int
        |> required "height" int

mediaListDecoder : Decoder (List Medium)
mediaListDecoder = Decode.list mediaDecoder
