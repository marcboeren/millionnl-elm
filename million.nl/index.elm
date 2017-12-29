
import Html exposing (Html, button, div, text, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time.DateTime as DateTime exposing (DateTime)
import Http
import Json.Decode as Decode
import Task

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type EntryType
  = Gallery
  | Story

type alias Entry =
  { timestamp : DateTime
  , entrytype : EntryType
  , title : String
  , slug : String
  , text : String
  , count : Int
  }

type alias Model =
  { entries : List Entry
  , activeEntry : Int
  , message : String
  }

-- INIT

init : (Model, Cmd Msg)
init =
  ( Model [] 0 "loading..."
  , getEntries
  )


-- UPDATE


type Msg
  = Next
  | Previous
  | Latest
  | Select (Int)
  | NewEntries (Result Http.Error (List Entry))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    _ = "comment"
    --_ = Debug.log "msg" (toString msg)
    --_ = Debug.log "model" (toString model)
  in
    case msg of
      Next ->
        if List.length model.entries == 0 then ({ model | activeEntry = 0}, Cmd.none)
        else if model.activeEntry == (List.length model.entries) - 1 then (model, Cmd.none)
        else ({ model | activeEntry = model.activeEntry + 1 }, Cmd.none)

      Previous ->
        if List.length model.entries == 0 then ({ model | activeEntry = 0}, Cmd.none)
        else if model.activeEntry == 0 then (model, Cmd.none)
        else ({ model | activeEntry = model.activeEntry - 1 }, Cmd.none)

      Latest ->
        if List.length model.entries == 0 then ({ model | activeEntry = 0}, Cmd.none)
        else ({ model | activeEntry = (List.length model.entries) - 1 }, Cmd.none)

      Select index ->
        if List.length model.entries == 0 then ({ model | activeEntry = 0}, Cmd.none)
        else if index < 0 || index >= (List.length model.entries) then ({ model | activeEntry = 0}, Cmd.none)
        else ({ model | activeEntry = index }, Cmd.none)

      NewEntries (Ok newEntries) ->
        (Model newEntries 0 "million.nl index loaded", Cmd.none)

      NewEntries (Err error) ->
        ({ model | message = toString error }, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Latest ]
      [ text "home" ]
    , viewEntries model
    , button [ onClick Previous ]
      [ text "previous" ]
    , button [ onClick Next ]
      [ text "next" ]
    , div []
      [ text model.message ]
    ]

viewEntries : Model -> Html Msg
viewEntries model =
  ul []
    (List.indexedMap (\index entry -> viewEntry entry index (index == model.activeEntry)) model.entries)

viewEntry : Entry -> Int -> Bool -> Html Msg
viewEntry entry index isActive =
  let
    possibleActiveEntry =
      if isActive then entry.title ++ " [*]"
      else entry.title
    typedPossibleActiveEntry =
      (toString entry.entrytype) ++ " - " ++ possibleActiveEntry
  in
    li [ onClick (Select index) ]
      [ text typedPossibleActiveEntry ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- HTTP


getEntries : Cmd Msg
getEntries =
  let
    url =
      "/json/index.json"
  in
    Http.send NewEntries (Http.get url decodeEntriesJson)

timestampDecoder : Decode.Decoder DateTime
timestampDecoder = Decode.succeed (DateTime.fromTuple (2003, 04, 30, 17, 54, 00, 0))

entrytypeDecoder : Decode.Decoder EntryType
entrytypeDecoder = Decode.succeed Gallery

--entrytypeDecoder = ("type" := Decode.string) `andThen` (Decode.succeed entrytypeStringDecoder)

--entrytypeStringDecoder : String -> EntryType
--entrytypeStringDecoder entrytype =
--  case entrytype of
--    "gallery" -> Gallery
--    "story" -> Story
--    _ -> Gallery


entryDecoder =
  Decode.map6 Entry
    (Decode.field "timestamp" timestampDecoder)
    (Decode.field "type" entrytypeDecoder)
    (Decode.field "title" Decode.string)
    (Decode.field "slug" Decode.string)
    (Decode.field "text" Decode.string)
    (Decode.field "count" Decode.int)

decodeEntriesJson : Decode.Decoder (List Entry)
decodeEntriesJson = Decode.list entryDecoder
