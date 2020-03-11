port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as H
import Html.Events exposing (onClick, onCheck)
import Html.Lazy exposing (lazy)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, string)
import Task
import Filesize
import Time
import Set
import Dict

import ScreenOverlay
import List.Extra exposing (groupWhile, unfoldr)
import Media exposing (..)

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

type alias DLOpts =
    { url : String
    , name : String
    , desc : String
    }

dloptsDecoder : Decoder (List DLOpts)
dloptsDecoder = Decode.list (Decode.map3 DLOpts (Decode.field "url" string)
                                 (Decode.field "name" string)
                                 (Decode.field "desc" string))

type alias RunningState =
    { media : List Medium
    , zone  : Time.Zone
    , overlay : ScreenOverlay.ScreenOverlay
    , current : (Maybe Medium, List DLOpts)
    , yearsChecked : Set.Set Int
    , yearsMap : Dict.Dict Int (List Medium)
    }

type Model
  = Failure Http.Error
  | Success RunningState

type Msg
  = SomeMedia (Result Http.Error (List Medium))
  | SomeDLOpts (Result Http.Error (List DLOpts))
  | ZoneHere Time.Zone
  | OpenOverlay Medium
  | CloseOverlay
  | CheckedYear Int Bool

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

mediaHTML : Time.Zone -> List (Medium, List Medium) -> List (Html Msg)
mediaHTML z ls = let oneDay (first, rest) =
                         let theDay = first.captured_at in
                         div [ H.class "aday" ]
                             (h2 [ ] [text (formatDay z theDay)]
                             :: List.map (mediumHTML z) (first::rest))
                 in List.map (lazy oneDay) ls

httpErrStr : Http.Error -> String
httpErrStr e = case e of
                   Http.BadUrl u -> "bad url: " ++ u
                   Http.Timeout -> "timeout"
                   Http.NetworkError -> "network error"
                   Http.BadStatus i -> "bad status: " ++ String.fromInt i
                   Http.BadBody b -> "bad body: " ++ b

htmlIf : Bool -> List (Html Msg) -> List (Html Msg)
htmlIf b l = if b then l else []

comma : Int -> String
comma i = let seg x = String.padLeft (if x >= 1000 then 3 else 0) '0' (String.fromInt (modBy 1000 x))
              parts = List.Extra.unfoldr (\x -> if x == 0
                                                then Nothing
                                                else Just (seg x, x // 1000)) i
          in String.join "," (List.reverse parts)

yearList : Set.Set Int -> List Int -> Html Msg
yearList checked years =
    div [ H.class "years" ]
        (List.concatMap (\y ->
                             [
                              input [ H.type_ "checkbox", H.id ("yr" ++ String.fromInt y),
                                      H.name (String.fromInt y),
                                      H.checked (Set.member y checked),
                                      onCheck (CheckedYear y)
                                    ] [],
                              label [ H.for ("yr" ++ String.fromInt y) ] [
                                   text (String.fromInt y) ]
                             ]
                        ) years)

renderMediaList : RunningState -> Html Msg
renderMediaList rs =
    let z = rs.zone
        filty = List.concatMap (\y -> Maybe.withDefault [] (Dict.get y rs.yearsMap))
                (List.reverse (Set.toList rs.yearsChecked))
        groupies = groupWhile (\a b -> formatDay z a.captured_at == formatDay z b.captured_at) filty
        totalSize = List.foldl (\x o -> x.file_size + o) 0 filty
        years = Dict.keys rs.yearsMap
    in
    div [ H.id "main" ]
        [
         div [ H.class "header" ]
             [ div [ ] [ text (comma (List.length filty)),
                         text " totaling ",
                         text (Filesize.format totalSize),
                         yearList rs.yearsChecked (List.reverse years)]],
         div [] [ScreenOverlay.overlayView rs.overlay CloseOverlay (renderOverlay z rs.current)],
         div [ H.class "media" ]
             (mediaHTML z groupies)]

view : Model -> Html Msg
view model =
    case model of
        Failure s ->
            pre [] [text ("I was unable to load the media: " ++ httpErrStr s)]

        Success rs ->
            if List.isEmpty rs.media then text "Loading..."
            else renderMediaList rs

dts : String -> Html Msg
dts s = dt [] [text s]

renderOverlay : Time.Zone -> (Maybe Medium, List DLOpts) -> Html Msg
renderOverlay z (mm, dls) = case mm of
                        Nothing -> text "wtf"
                        Just m -> div [ H.class "details" ]
                                  ([h2 [] [ text (m.id) ]
                                   , img [ H.src ("/thumb/" ++ m.id) ] []
                                   , dl [ H.class "deets" ] [
                                         h2 [] [text "Details" ]
                                        , dts "Captured"
                                        , dd [ H.title (String.fromInt (Time.posixToMillis m.captured_at))]
                                             [text <| formatDay z m.captured_at ++ " " ++ formatTime z m.captured_at]
                                        , dts "Camera Model"
                                        , dd [] [text m.camera_model]
                                        , dts "Dims"
                                        , dd [] [text (String.fromInt m.width ++ "x" ++ String.fromInt m.height)]
                                        , dts "Size"
                                        , dd [] [text <| String.fromInt m.file_size ]
                                        , dts "Type"
                                        , dd [] [ text m.media_type ]
                                        ]
                                   ] ++ htmlIf (not (List.isEmpty dls))
                                        [ul [ H.class "dls" ]
                                            (h2 [] [text "Downloads"]
                                            :: List.map (\d -> li []
                                                             [a [ H.href d.url, H.title d.desc] [text d.name]])
                                                dls)])


init : () -> (Model, Cmd Msg)
init _ =
  ( Success emptyState
  , Cmd.batch [Http.get
                   { url = "/api/media"
                   , expect = Http.expectJson SomeMedia mediaListDecoder
                   },
                   Task.perform ZoneHere Time.here]
  )

emptyState : RunningState
emptyState = RunningState [] Time.utc ScreenOverlay.initOverlay (Nothing, []) Set.empty Dict.empty

updRunning : Model -> RunningState
updRunning m = case m of
                   Success r -> r
                   _ -> emptyState

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SomeMedia result ->
            case result of
                Ok meds ->
                    let r = updRunning model
                        z = r.zone
                        ymap = List.foldl (\x o -> Dict.update (Time.toYear z x.captured_at)
                                                   (\e -> Just <| case e of
                                                                      Nothing -> [x]
                                                                      Just l -> l ++ [x])
                                                       o) Dict.empty meds
                        maxYear = case List.maximum (Dict.keys ymap) of
                                      Nothing -> Set.empty
                                      Just x -> Set.singleton x
                    in
                    (Success {r | media = meds,
                                  yearsChecked = maxYear,
                                  yearsMap = ymap}, Cmd.none)

                Err x ->
                    (Failure x, Cmd.none)

        SomeDLOpts result ->
            case result of
                Ok dls ->
                    let r = updRunning model
                        (m, _) = r.current in
                    (Success {r | current = (m, dls)}, Cmd.none)

                Err x ->
                    let fakedls = [DLOpts "" ("error fetching downloads: " ++ httpErrStr x) ""]
                        r = updRunning model
                        (m, _) = r.current in
                    (Success {r | current = (m, fakedls)}, Cmd.none)

        ZoneHere z ->
            let r = updRunning model in
            (Success {r | zone = z}, Cmd.none)

        OpenOverlay m ->
            let r = updRunning model in
            (Success { r | overlay = ScreenOverlay.show r.overlay, current = (Just m, []) },
                 Cmd.batch [lockScroll Nothing,
                            Http.get
                                { url = "/api/retrieve2/" ++ m.id
                                , expect = Http.expectJson SomeDLOpts dloptsDecoder
                           }])

        CloseOverlay ->
            let r = updRunning model in
            (Success { r | overlay = ScreenOverlay.hide r.overlay }, unlockScroll Nothing )

        CheckedYear y checked ->
            let r = updRunning model in
            (Success { r | yearsChecked = (if checked then Set.insert else Set.remove) y r.yearsChecked },
             Cmd.none)


main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
