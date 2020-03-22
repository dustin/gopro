port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as H
import Html.Events exposing (onClick, onCheck)
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as Decode exposing (Decoder, int, string)
import Task
import Filesize
import Time
import Set
import Dict
import DateRangePicker as Picker
import DateRangePicker.Range as Range exposing (beginsAt, endsAt)
import Time.Extra as TE

import ScreenOverlay
import List.Extra exposing (groupWhile, minimumBy, maximumBy)
import Media exposing (..)
import Formats as F

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
    , cameras : List String
    , types : List String
    , years : Set.Set Int
    , filty : List Medium
    }

type Model = Model
    { httpError : Maybe Http.Error
    , zone  : Time.Zone
    , overlay : ScreenOverlay.ScreenOverlay
    , current : (Maybe Medium, Maybe (Result Http.Error DLOpts))
    , datePicker : Picker.State
    , camerasChecked : Set.Set String
    , typesChecked : Set.Set String
    , media : Maybe Media
    , filters : List (Model -> Medium -> Bool)
    }

emptyState : Model
emptyState = Model
             { httpError = Nothing
             , zone = Time.utc
             , overlay = ScreenOverlay.initOverlay
             , current = (Nothing, Nothing)
             , datePicker = let cfg = Picker.defaultConfig in
                            Picker.init {cfg | noRangeCaption = "All Time",
                                               allowFuture = False} Nothing
             , camerasChecked = Set.empty
             , typesChecked = Set.empty
             , media = Nothing
             , filters = [dateFilter, camFilter, typeFilter]}

type Msg
  = SomeMedia (Result Http.Error (List Medium))
  | SomeDLOpts (Result Http.Error DLOpts)
  | ZoneHere Time.Zone
  | CurrentTime Time.Posix
  | OpenOverlay Medium
  | CloseOverlay
  | CheckedCam String Bool
  | CheckedType String Bool
  | PickerChanged Picker.State
  | YearClicked Int

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

-- Render a list of checkboxes and the model manipulation bits.
aList : Set.Set comparable           -- Checked state
      -> List comparable             -- All options
      -> String                      -- CSS class of the container
      -> (comparable -> String)      -- Representation function
      -> (comparable -> Bool -> Msg) -- Callback message
      -> Html Msg
aList checked things class rep msg =
    div [ H.class class ]
        (List.concatMap (\t -> let id = String.replace " " "_" (rep t) in
                               [ input [ H.type_ "checkbox", H.id (class ++ id),
                                         H.name id,
                                         H.checked (Set.member t checked),
                                         onCheck (msg t)
                                       ] []
                               , label [ H.for (class ++ id) ] [ text (rep t) ]
                               ]
                        ) things)

renderMediaList : Media -> Model -> Html Msg
renderMediaList ms (Model model) =
    let z = model.zone
        groupies = groupWhile (\a b -> F.day z a.captured_at == F.day z b.captured_at) ms.filty
        totalSize = List.foldl (\x o -> x.file_size + o) 0 ms.filty
        aYear y = div [ H.class "year" ] [
                   a [ onClick (YearClicked y) ] [ text (String.fromInt y) ]
                  ]
    in
    div [ H.id "main" ]
        [
         div [ H.class "header" ]
             [ div [ ] [ text (F.comma (List.length ms.filty)),
                         text " totaling ",
                         text (Filesize.format totalSize),
                         div [ H.class "datepick" ]
                             ([ Picker.view PickerChanged model.datePicker,
                                    div [ H.class "year" ] [ text "Quick year picker:" ] ]
                             ++ (List.map aYear (List.reverse (Set.toList ms.years)))),
                         aList model.camerasChecked ms.cameras "cameras" identity CheckedCam,
                         aList model.typesChecked ms.types "types" identity CheckedType
                       ]
             ],
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

renderMetaData : MetaData -> List (Html Msg)
renderMetaData g = (case (g.lat, g.lon) of
                        (Just lat, Just lon) -> [dts "Location",
                                                 dd []
                                                 [a [ H.href ("https://www.google.com/maps/search/?api=1&query=" ++
                                                              String.fromFloat lat ++ "," ++
                                                              String.fromFloat lon)]
                                                      [ text (String.fromFloat lat ++ "," ++
                                                              String.fromFloat lon) ]]]
                        _ -> [])
               ++ (case g.scene of
                      Nothing -> []
                      Just c -> [ dts "Scene",
                                  dd [ ]
                                  [ span [ H.class "scene" ] [text (locationStr c)],
                                    text (" with probability " ++
                                          String.fromFloat (Maybe.withDefault 0 g.sceneProb)) ]])

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
                        ] ++ (case m.source_duration of
                                  Nothing -> []
                                  Just x -> [ dts "Duration"
                                            , dd [] [text (F.millis (Maybe.withDefault 0 m.source_duration))]])
                          ++ (case m.metaData of
                                  Nothing -> []
                                  Just g -> renderMetaData g))
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

filter : Model -> Model
filter (Model model) =
    case model.media of
        Nothing -> Model model
        Just ms ->
            let z = model.zone
                filty = List.filter (\m -> List.all (\f -> f (Model model) m) model.filters) ms.media in
            Model { model | media = Just { ms | filty = filty }}

camFilter : Model -> Medium -> Bool
camFilter (Model model) m = Set.member m.camera_model model.camerasChecked

typeFilter : Model -> Medium -> Bool
typeFilter (Model model) m = Set.member (mediaTypeStr m.media_type) model.typesChecked

dateFilter : Model -> Medium -> Bool
dateFilter (Model model) m =
    let mr = Picker.getRange model.datePicker
        lte a b = Time.posixToMillis a <= Time.posixToMillis b
    in
    case mr of
        Nothing -> True
        Just r -> lte (beginsAt r) m.captured_at && lte m.captured_at (endsAt r)

addOrRemove : Bool -> comparable -> Set.Set comparable -> Set.Set comparable
addOrRemove b = if b then Set.insert else Set.remove

truncDay : Time.Posix -> Time.Posix
truncDay t = let m = Time.posixToMillis t in
             Time.millisToPosix (m - modBy 86400000 m)

startOfPreviousMonth : Time.Zone -> Time.Posix -> Time.Posix
startOfPreviousMonth zone = TE.startOfMonth zone >> TE.addMillis -1 >> TE.startOfMonth zone

startOfYear : Time.Zone -> Time.Posix -> Time.Posix
startOfYear zone = TE.setMonth zone Time.Jan >> TE.startOfMonth zone

startOfPreviousYear : Time.Zone -> Time.Posix -> Time.Posix
startOfPreviousYear zone =
    TE.setMonth zone Time.Jan >> TE.startOfMonth zone >> TE.addMillis -1 >> startOfYear zone

myRanges : Time.Zone -> Time.Posix -> List ( String, Range.Range )
myRanges zone today =
    let
        daysBefore n posix =
            posix |> TE.addDays -n |> TE.startOfDay zone
    in
    [ ( "Last 7 days", Range.create zone (today |> daysBefore 7) (today |> TE.startOfDay zone |> TE.addMillis -1))
    , ( "Last 30 days", Range.create zone (today |> daysBefore 30) (today |> TE.startOfDay zone |> TE.addMillis -1))
    , ( "This month", Range.create zone (today |> TE.startOfMonth zone) today)
    , ( "Last month", Range.create zone (today |> startOfPreviousMonth zone) (today |> TE.startOfMonth zone |> TE.addMillis -1))
    , ( "This year", Range.create zone (today |> startOfYear zone) today)
    , ( "Last year", Range.create zone (today |> startOfPreviousYear zone) (today |> startOfYear zone))
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg (Model model) =
    case msg of
        SomeMedia result ->
            case result of
                Ok meds ->
                    let z = model.zone
                        cameras = Set.fromList (List.map .camera_model meds)
                        types = Set.fromList (List.map (\m -> mediaTypeStr m.media_type) meds)
                        years = Set.fromList <| List.map (Time.toYear z << .captured_at) meds
                    in
                    (filter (Model {model | media = Just (Media meds (Set.toList cameras) (Set.toList types) years []),
                                            camerasChecked = cameras,
                                            typesChecked = types
                                   }), Cmd.none)

                Err x ->
                    (Model {model | httpError = Just  x}, Cmd.none)

        SomeDLOpts result -> let (m, _) = model.current in
                             (Model {model | current = (m, Just result)}, Cmd.none)

        CurrentTime t ->
            (Model model,
                 let sevenDays = TE.addDays -7 t
                     recond = Picker.reconfigure (\c -> { c | predefinedRanges = myRanges }) model.datePicker
                     rangedPicker = Picker.setRange (Just (Range.create model.zone sevenDays t)) recond
                 in
                 Picker.now PickerChanged rangedPicker)


        ZoneHere z ->
            (Model {model | zone = z}, Task.perform CurrentTime Time.now)

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

        PickerChanged state ->
            ( filter (Model { model | datePicker = state } ), Cmd.none )

        CheckedCam c checked ->
            (filter (Model { model | camerasChecked = addOrRemove checked c model.camerasChecked }),
             Cmd.none)

        CheckedType t checked ->
            (filter (Model { model | typesChecked = addOrRemove checked t model.typesChecked }),
             Cmd.none)

        YearClicked y -> let b = String.fromInt y ++ "-01-01T00:00:00"
                             e = String.fromInt y ++ "-12-31T23:59:59"
                             eb = TE.fromIso8601Date model.zone b
                             et = TE.fromIso8601Date model.zone e
                             nst = case (eb, et) of
                                       (Just l, Just h) ->
                                           Picker.setRange (Just (Range.create model.zone l h)) model.datePicker
                                       _ -> model.datePicker
                         in
                             (filter (Model {model | datePicker = nst}), Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions (Model model) =
     Picker.subscriptions PickerChanged model.datePicker


main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
