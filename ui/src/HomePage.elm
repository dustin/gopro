port module HomePage exposing (main)

import Html exposing (..)
import Browser
import Time
import Html.Events exposing (onClick)
import Iso8601
import Html.Lazy exposing (lazy)
import Task
import ScreenOverlay
import ListExtra exposing (groupWhile)
import Html.Attributes as H
import Json.Decode as Decode exposing (Decoder, decodeString, null, int, string, float)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Http

{-
id: "0r1LLLDBPPLVd",
camera_model: "HERO8 Black",
captured_at: "2020-01-11T16:43:44Z",
created_at: "2020-01-13T00:48:07Z",
file_size: 8323068,
moments_count: 0,
ready_to_view: "ready",
resolution: "12000000",
source_duration: null,
type: "Photo",
token: "",
width: 4000,
height: 3000
-}

port lockScroll : Maybe String -> Cmd msg
port unlockScroll : Maybe String -> Cmd msg


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

type alias RunningState =
    { media : List Medium
    , zone  : Time.Zone
    , overlay : ScreenOverlay.ScreenOverlay
    , current : Maybe Medium
    }
                   
type Model
  = Failure Http.Error
  | Loading
  | Success RunningState

type Msg
  = SomeMedia (Result Http.Error (List Medium))
  | ZoneHere Time.Zone
  | OpenOverlay Medium
  | CloseOverlay
    
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

monthStr : Time.Month -> String
monthStr month =
  case month of
    Time.Jan -> "Jan"
    Time.Feb -> "Feb"
    Time.Mar -> "Mar"
    Time.Apr -> "Apr"
    Time.May -> "May"
    Time.Jun -> "Jun"
    Time.Jul -> "Jul"
    Time.Aug -> "Aug"
    Time.Sep -> "Sep"
    Time.Oct -> "Oct"
    Time.Nov -> "Nov"
    Time.Dec -> "Dec"

-- padLeft 5 '.' "1"   == "....1"
formatTime : Time.Zone -> Time.Posix -> String
formatTime z t = String.fromInt (Time.toHour z t) ++ ":" ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute z t))

formatDay : Time.Zone -> Time.Posix -> String
formatDay z t = let two x = String.padLeft 2 '0' (String.fromInt x) in
                String.fromInt (Time.toYear z t) ++ "-" ++ monthStr (Time.toMonth z t) ++ "-" ++ two (Time.toDay z t)

mediumHTML : Time.Zone -> Medium -> Html Msg
mediumHTML z m = div [ H.class "medium", onClick (OpenOverlay m) ] [
                  div [ H.class "time" ] [
                       text (formatTime z m.captured_at),
                       span [ H.class "dims" ] [ text (String.fromInt m.width ++ "x" ++ String.fromInt m.height) ]
                      ],
                      img [ H.class "thumb", H.src ("/thumb/" ++ m.id) ] []
                 ]

mediaHTML : Time.Zone -> List (List Medium) -> List (Html Msg)
mediaHTML z ls = let oneDay d =
                         let theDay = case List.head d of
                                         Just first -> first.captured_at
                                         Nothing -> Time.millisToPosix 0
                         in
                         div [ H.class "aday" ]
                             (h2 [ ] [text (formatDay z theDay)]
                             :: List.map (mediumHTML z) d)
                 in List.map (lazy oneDay) ls

httpErrStr : Http.Error -> String
httpErrStr e = case e of
                   Http.BadUrl u -> "bad url: " ++ u
                   Http.Timeout -> "timeout"
                   Http.NetworkError -> "network error"
                   Http.BadStatus i -> "bad status: " ++ String.fromInt i
                   Http.BadBody b -> "bad body: " ++ b

view : Model -> Html Msg
view model =
    case model of
        Failure s ->
            pre [] [text ("I was unable to load the media: " ++ httpErrStr s)]

        Loading ->
            text "Loading..."

        Success rs ->
            let z = rs.zone
                groupies = groupWhile (\a b -> formatDay z a.captured_at == formatDay z b.captured_at) rs.media
            in
                if List.isEmpty rs.media then text "Loading..."
                else div [ H.id "main" ]
                    [div [ H.class "media" ]
                         (mediaHTML z groupies ++
                              [ScreenOverlay.overlayView rs.overlay CloseOverlay (renderOverlay z rs.current)])]


renderOverlay : Time.Zone -> Maybe Medium -> Html Msg
renderOverlay z mm = case mm of
                        Nothing -> text "wtf"
                        Just m -> div [ H.class "details" ]
                                  [h2 [] [ text (m.id) ]
                                  , img [ H.src ("/thumb/" ++ m.id) ] []
                                  , dl [ H.class "deets" ] [
                                        dt [] [text "Captured"]
                                       , dd [] [text <| formatDay z m.captured_at ++ " " ++ formatTime z m.captured_at]
                                       , dt [] [text "Camera Model"]
                                       , dd [] [text m.camera_model]
                                       , dt [] [text "Dims"]
                                       , dd [] [text (String.fromInt m.width ++ "x" ++ String.fromInt m.height)]
                                       , dt [] [text "Size"]
                                       , dd [] [text <| String.fromInt m.file_size ]
                                       , dt [] [text "Type"]
                                       , dd [] [ text m.media_type ]
                                       ]
                                  ]

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Cmd.batch [Http.get
                   { url = "/api/media"
                   , expect = Http.expectJson SomeMedia mediaListDecoder
                   },
                   Task.perform ZoneHere Time.here]
  )

timeDown : Medium -> Medium -> Order
timeDown a b = compare (Time.posixToMillis b.captured_at) (Time.posixToMillis a.captured_at)

updRunning : Model -> RunningState
updRunning m = case m of
                   Success r -> r
                   _ -> RunningState [] Time.utc ScreenOverlay.initOverlay Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SomeMedia result ->
            case result of
                Ok meds ->
                    let r = updRunning model in
                    (Success {r | media = meds}, Cmd.none)

                Err x ->
                    (Failure x, Cmd.none)
                        
        ZoneHere z ->
            let r = updRunning model in
            (Success {r | zone = z}, Cmd.none)

        OpenOverlay m ->
            let r = updRunning model in
            (Success { r | overlay = ScreenOverlay.show r.overlay, current = Just m }, lockScroll Nothing )

        CloseOverlay ->
            let r = updRunning model in
            (Success { r | overlay = ScreenOverlay.hide r.overlay }, unlockScroll Nothing )

main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
