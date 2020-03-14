port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as H
import Html.Events exposing (onClick, onCheck)
import Html.Lazy exposing (lazy)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, int, string)
import Task
import Filesize
import Time
import Set
import Dict

import ScreenOverlay
import List.Extra exposing (groupWhile)
import Media exposing (..)
import Formats as F

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

type alias DLOpt =
    { url : String
    , name : String
    , desc : String
    , width : Int
    , height : Int
    }

dloptDecoder : Decoder (List DLOpt)
dloptDecoder = Decode.list (Decode.map5 DLOpt
                                (Decode.field "url" string)
                                (Decode.field "name" string)
                                (Decode.field "desc" string)
                                (Decode.field "width" int)
                                (Decode.field "height" int))

type alias DLOpts =
    { default : DLOpt
    , list : List DLOpt
    }

dloptsDecoder : Decoder DLOpts
dloptsDecoder = Decode.map (\opts ->
                                let ixd = Dict.fromList (List.map (\d -> (d.name, d)) opts)
                                    best = case Dict.get "mp4_low" ixd of
                                               Just d -> d
                                               Nothing -> case List.head opts of
                                                              Nothing -> DLOpt "" "" "" 0 0
                                                              Just d -> d
                                in
                                DLOpts best opts
                           ) dloptDecoder

type alias Media =
    { media : List Medium
    , years : Set.Set Int
    , filty : List Medium
    }

type Model = Model
    { httpError : Maybe Http.Error
    , zone  : Time.Zone
    , overlay : ScreenOverlay.ScreenOverlay
    , current : (Maybe Medium, Maybe (Result Http.Error DLOpts))
    , yearsChecked : Set.Set Int
    , media : Maybe Media
    , filters : List (Model -> Medium -> Bool)
    }

type Msg
  = SomeMedia (Result Http.Error (List Medium))
  | SomeDLOpts (Result Http.Error DLOpts)
  | ZoneHere Time.Zone
  | OpenOverlay Medium
  | CloseOverlay
  | CheckedYear Int Bool

mediumHTML : Time.Zone -> Medium -> Html Msg
mediumHTML z m = div [ H.class "medium", onClick (OpenOverlay m) ] [
                      img [ H.class "thumb", H.src ("/thumb/" ++ m.id) ] [],
                      case m.source_duration of
                          Nothing -> text ""
                          Just t -> span [ H.class "duration" ] [ text (F.millis t) ]
                 ]

mediaHTML : Time.Zone -> List (Medium, List Medium) -> List (Html Msg)
mediaHTML z ls = let oneDay (first, rest) =
                         let theDay = first.captured_at in
                         div [ H.class "aday" ]
                             (h2 [ ] [text (F.day z theDay)]
                             :: List.map (mediumHTML z) (first::rest))
                 in List.map (lazy oneDay) ls
             
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

renderMediaList : Media -> Model -> Html Msg
renderMediaList ms (Model model) =
    let z = model.zone
        groupies = groupWhile (\a b -> F.day z a.captured_at == F.day z b.captured_at) ms.filty
        totalSize = List.foldl (\x o -> x.file_size + o) 0 ms.filty
        years = Set.toList ms.years
    in
    div [ H.id "main" ]
        [
         div [ H.class "header" ]
             [ div [ ] [ text (F.comma (List.length ms.filty)),
                         text " totaling ",
                         text (Filesize.format totalSize),
                         yearList model.yearsChecked (List.reverse years)]],
         div [] [ScreenOverlay.overlayView model.overlay CloseOverlay (renderOverlay z model.current)],
         div [ H.class "media" ]
             (mediaHTML z groupies)]

view : Model -> Html Msg
view (Model model) =
    case model.httpError of
        Nothing ->
            case model.media of
                Nothing -> text "Loading..."
                Just m -> renderMediaList m (Model model)
        Just x -> pre [] [text ("I was unable to load the media: " ++ F.httpErr x)]


dts : String -> Html Msg
dts s = dt [] [text s]

renderIcon : Medium -> Maybe (Result Http.Error DLOpts) -> Html Msg
renderIcon m mdls =
    let thumbUrl = "/thumb/" ++ m.id
        thumb = img [ H.src thumbUrl ] []
        still = List.member m.media_type [Photo, Burst, TimeLapse] in
    if still
    then thumb
    else case mdls of
             Just (Ok dls) -> (video [ H.controls True, H.autoplay True, H.poster thumbUrl,
                                            H.width dls.default.width, H.height dls.default.height ]
                                    [source [ H.src dls.default.url, H.type_ "video/mp4" ] []])
             _ -> thumb

renderOverlay : Time.Zone -> (Maybe Medium, Maybe (Result Http.Error DLOpts)) -> Html Msg
renderOverlay z (mm, mdls) =
    case mm of
        Nothing -> text "nothing to see here"
        Just m -> div [ H.class "details" ]
                  ([h2 [] [ text (m.id) ]
                   , renderIcon m mdls
                   , dl [ H.class "deets" ] ([
                         h2 [] [text "Details" ]
                        , dts "Captured"
                        , dd [ H.title (String.fromInt (Time.posixToMillis m.captured_at))]
                             [text <| F.day z m.captured_at ++ " " ++ F.time z m.captured_at]
                        , dts "Camera Model"
                        , dd [] [text m.camera_model]
                        , dts "Dims"
                        , dd [] [text (String.fromInt m.width ++ "x" ++ String.fromInt m.height)]
                        , dts "Size"
                        , dd [ H.title (F.comma m.file_size) ] [text <| Filesize.format m.file_size ]
                        , dts "Type"
                        , dd [] [ text (mediaTypeStr m.media_type) ]
                        , dts "Ready State"
                        , dd [] [ text (readyTypeStr m.ready_to_view) ]
                        ] ++ case m.source_duration of
                                 Nothing -> []
                                 Just x -> [ dts "Duration"
                                           , dd [] [text (F.millis (Maybe.withDefault 0 m.source_duration))]])
                   ] ++ case mdls of
                            Nothing -> []
                            Just (Err err) -> [div [ H.class "dls" ]
                                                   [ h2 [] [ text "Error" ]
                                                   , div [] [ text "Error fetching downloads: "
                                                            , text (F.httpErr err) ]]]
                            Just (Ok dls) ->
                                [ul [ H.class "dls" ]
                                     (h2 [] [text "Downloads"]
                                     :: List.map (\d -> li []
                                                      [a [ H.href d.url, H.title d.desc] [text d.name]])
                                         dls.list)])


init : () -> (Model, Cmd Msg)
init _ =
  ( emptyState
  , Cmd.batch [Http.get
                   { url = "/api/media"
                   , expect = Http.expectJson SomeMedia mediaListDecoder
                   },
                   Task.perform ZoneHere Time.here]
  )

emptyState : Model
emptyState = Model
             { httpError = Nothing
             , zone = Time.utc
             , overlay = ScreenOverlay.initOverlay
             , current = (Nothing, Nothing)
             , yearsChecked = Set.empty
             , media = Nothing
             , filters = [yearFilter]}

filter : Model -> Model
filter (Model model) =
    case model.media of
        Nothing -> Model model
        Just ms ->
            let z = model.zone
                filty = List.filter (\m -> List.all (\f -> f (Model model) m) model.filters) ms.media in
            Model { model | media = Just { ms | filty = filty }}

yearFilter : Model -> Medium -> Bool
yearFilter (Model model) m =
    let z = model.zone in
    Set.member (Time.toYear z m.captured_at) model.yearsChecked

update : Msg -> Model -> (Model, Cmd Msg)
update msg (Model model) =
    case msg of
        SomeMedia result ->
            case result of
                Ok meds ->
                    let z = model.zone
                        years = Set.fromList (List.map (\m -> Time.toYear z m.captured_at) meds)
                        maxYear = case List.maximum (Set.toList years) of
                                      Nothing -> Set.empty
                                      Just x -> Set.singleton x
                                                
                    in
                    (filter (Model {model | media = Just (Media meds years []),
                                            yearsChecked = maxYear}), Cmd.none)

                Err x ->
                    (Model {model | httpError = Just  x}, Cmd.none)

        SomeDLOpts result -> let (m, _) = model.current in
                             (Model {model | current = (m, Just result)}, Cmd.none)

        ZoneHere z ->
            (Model {model | zone = z}, Cmd.none)

        OpenOverlay m ->
            (Model { model | overlay = ScreenOverlay.show model.overlay, current = (Just m, Nothing) },
             Cmd.batch [lockScroll Nothing,
                        Http.get
                            { url = "/api/retrieve2/" ++ m.id
                            , expect = Http.expectJson SomeDLOpts dloptsDecoder
                            }])

        CloseOverlay ->
            (Model { model | overlay = ScreenOverlay.hide model.overlay,
                             current = (Nothing, Nothing) }, unlockScroll Nothing )

        CheckedYear y checked ->
            (filter (Model { model | yearsChecked = (if checked then Set.insert else Set.remove) y model.yearsChecked }),
             Cmd.none)


main = Browser.element
    { init = init
    , update = update
    , subscriptions = always Sub.none
    , view = view
    }
