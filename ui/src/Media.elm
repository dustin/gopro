module Media exposing (MediaType(..), ReadyType(..), MetaData, Location(..),
                           Medium, mediaTypeStr, readyTypeStr,
                           mediaDecoder, mediaListDecoder, mediaPoint,
                           locationStr
                      )

import Iso8601
import Time
import Json.Decode as Decode exposing (Decoder, int, string, nullable, float)
import Json.Decode.Pipeline exposing (required, optional)
import Geo exposing (..)

type MediaType = Photo
    | Video
    | TimeLapse
    | TimeLapseVideo
    | Burst
    | Chaptered
    | Livestream
    | Looped
    | LoopedVideo
    | BurstVideo
    | Continuous
    | ExternalVideo
    | Session
    | MultiClipEdit
    | Unknown


mediaTypeStr t = case t of
                     Photo -> "Photo"
                     Video -> "Video"
                     TimeLapse -> "TimeLapse"
                     TimeLapseVideo -> "TimeLapseVideo"
                     Burst -> "Burst"
                     Chaptered -> "Chaptered"
                     Livestream -> "Livestream"
                     Looped -> "Looped"
                     LoopedVideo -> "LoopedVideo"
                     BurstVideo -> "BurstVideo"
                     Continuous -> "Continuous"
                     ExternalVideo -> "ExternalVideo"
                     Session -> "Session"
                     MultiClipEdit -> "MultiClipEdit"
                     Unknown -> "Unknown"

strMediaType s = case s of
                     "Photo" -> Photo
                     "Video" -> Video
                     "TimeLapse" -> TimeLapse
                     "TimeLapseVideo" -> TimeLapseVideo
                     "Burst" -> Burst
                     "Chaptered" -> Chaptered
                     "Livestream" -> Livestream
                     "Looped" -> Looped
                     "LoopedVideo" -> LoopedVideo
                     "BurstVideo" -> BurstVideo
                     "Continuous" -> Continuous
                     "ExternalVideo" -> ExternalVideo
                     "Session" -> Session
                     "MultiClipEdit" -> MultiClipEdit
                     _ -> Unknown


type ReadyType = Ready | Transcoding | Uploading | Failure | UnknownReadyType

readyTypeStr r = case r of
                     Ready -> "Ready"
                     Transcoding -> "Transcoding"
                     Uploading -> "Uploading"
                     Failure -> "Failure"
                     UnknownReadyType -> "Unknown ready type"

strReadyType s = case s of
                     "ready" -> Ready
                     "transcoding" -> Transcoding
                     "uploading" -> Uploading
                     "failure" -> Failure
                     _ -> UnknownReadyType
type Location = Snow | Urban | Indoor | Water | Vegetation | Beach | UnknownLocation

locationStr l = case l of
                    Snow -> "Snow"
                    Urban -> "Urban"
                    Indoor -> "Indoor"
                    Water -> "Water"
                    Vegetation -> "Vegetation"
                    Beach -> "Beach"
                    UnknownLocation -> "Unknown"

strLocation l = case l of
                    "Snow" -> Snow
                    "Urban" -> Urban
                    "Indoor" -> Indoor
                    "Water" -> Water
                    "Vegetation" -> Vegetation
                    "Beach" -> Beach
                    _ -> UnknownLocation

type alias MetaData =
    { cam : String
    , ts : Maybe Time.Posix
    , lat : Maybe Float
    , lon : Maybe Float
    , maxSpeed2d : Maybe Float
    , maxSpeed3d : Maybe Float
    , maxFaces : Int
    , scene : Maybe Location
    , sceneProb : Maybe Float
    , maxDistance : Maybe Float
    , totalDistance : Maybe Float
    }

metaDataDecoder : Decoder MetaData
metaDataDecoder =
    Decode.succeed MetaData
        |> required "camera" string
        |> required "ts" (nullable Iso8601.decoder)
        |> required "lat" (nullable float)
        |> required "lon" (nullable float)
        |> required "maxSpeed2d" (nullable float)
        |> required "maxSpeed3d" (nullable float)
        |> required "maxFaces" int
        |> required "scene" (nullable (Decode.map strLocation string))
        |> required "sceneProb" (nullable float)
        |> optional "maxDistance" (nullable float)  Nothing
        |> optional "totalDistance" (nullable float) Nothing

type alias Medium =
    { id : String
    , camera_model : String
    , captured_at : Time.Posix
    , created_at : Time.Posix
    , file_size : Maybe Int
    , moments_count : Int
    , ready_to_view : ReadyType
    , source_duration : Maybe Int
    , media_type : MediaType
    , token : String
    , width : Int
    , height : Int
    , metaData : Maybe MetaData
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
        |> optional "file_size" (nullable int) Nothing
        |> required "moments_count" int
        |> required "ready_to_view" (Decode.map strReadyType string)
        |> required "source_duration" stringInt
        |> required "type" (Decode.map strMediaType string)
        |> required "token" string
        |> optional "width" int 1920
        |> optional "height" int 1080
        |> optional "meta_data" (nullable metaDataDecoder) Nothing

mediaListDecoder : Decoder (List Medium)
mediaListDecoder = Decode.list mediaDecoder

mediaPoint : Medium -> Maybe Point
mediaPoint m = Maybe.andThen (\md -> case (md.lat, md.lon) of
                                         (Just lat, Just lon) -> Just (Point lat lon)
                                         _ -> Nothing) m.metaData
