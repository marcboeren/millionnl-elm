
import Html exposing (Html, Attribute, node, div, span, text, h1, a, button, img, time, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time.DateTime as DateTime exposing (DateTime)
import Http
import Json.Decode as Decode
import Task
import String exposing (padLeft)
import Json.Encode exposing (string)

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
        else ({ model | activeEntry = 0 }, Cmd.none)

      Select index ->
        if List.length model.entries == 0 then ({ model | activeEntry = 0}, Cmd.none)
        else if index < 0 || index >= (List.length model.entries) then ({ model | activeEntry = 0}, Cmd.none)
        else ({ model | activeEntry = index }, Cmd.none)

      NewEntries (Ok newEntries) ->
        (Model newEntries 0 "million.nl index loaded", Cmd.none)

      NewEntries (Err error) ->
        ({ model | message = toString error }, Cmd.none)


-- VIEW

style : List (Attribute msg) -> List (Html msg) -> Html msg
style attributes children =
    node "style" attributes children

unselectable : String -> Attribute msg
unselectable value =
  attribute "unselectable" value

view : Model -> Html Msg
view model =
  div []
    [ style []
      [ text """
@font-face {
    font-family: 'GoodFootRegular';
    src: url('http://www.million.nl/css/goodfoot-webfont.eot');
    src: url('http://www.million.nl/css/goodfoot-webfont.eot?#iefix') format('embedded-opentype'),
         url('http://www.million.nl/css/goodfoot-webfont.woff') format('woff'),
         url('http://www.million.nl/css/goodfoot-webfont.ttf') format('truetype'),
         url('http://www.million.nl/css/goodfoot-webfont.svg#GoodFootRegular') format('svg');
    font-weight: normal;
    font-style: normal;
}

* { -moz-box-sizing: border-box; -webkit-box-sizing: border-box; box-sizing: border-box; }

html, body, div, span,
h1, h2, h3, p, blockquote, pre,
code, em, img,
small, strong, sub, sup, b, i, ol, ul, li,
table, tbody, tr, th, td {
  margin: 0;
  padding: 0;
  border: 0;
  font-size: 100%;
  font: inherit;
  vertical-align: baseline;
  position: relative;
}
a:hover, a:active { outline: none; }

body { background: #333 url(http://www.million.nl/css/blackboard.jpg) top center repeat; color: #fff; text-align: center; position: relative; font-family: "AvenirNextCondensed-Regular", "GillSans-Light", Helvetica, sans-serif; font-size: 14px; }
a { color: #fff; text-decoration: none; }
a:visited, a:hover, a:active { color: #fff; }
p { line-height: 1.5; }
h1 { z-index: 4; padding-top: 0.2em; font-size: 40px; }
h1 { font-family: GoodFootRegular, Helvetica, sans-serif; font-style: normal; font-weight: normal; }

h2 { z-index: 3; padding-top: 0.2em; font-size: 22px; line-height: 1em; }
h2 { font-family: GoodFootRegular, Helvetica, sans-serif; font-style: normal; font-weight: normal; }
h2 span.date { font-size: 15px; }

#head { z-index: 3; }
#content { z-index: 2; overflow-x: hidden; }
#c { z-index: 1; color: rgba(255,255,255,0.4); font-family: GoodFootRegular, Helvetica, sans-serif; font-size: 15px; }
#c a, #c a:visited, #c a:active, #c a:hover { color: rgba(255,255,255,0.4); }

.view { position: relative; overflow: hidden; width: 100%; height: 100%; }

.items { white-space: nowrap; height: auto; user-select: none; }
.items .item { position: relative; white-space: normal; }

.group { width: 170px; position: relative; margin: 30px auto; cursor: pointer;}
.group img.bck { position: absolute; width: 240px; height: 120%; top: -30px; left: -35px; }
.group time { color: rgba(255,255,255,0.6); }
.group p { padding: 3px 5px 0; }
.group .polaroid a, .group .polaroid a:visited, .group .polaroid a:hover, .group .polaroid a:active { color: #222; }

#content.story { color: #ccc; text-align: left; max-width: 600px; padding: 0 10px; margin: 0 auto; position: relative; }
.story h2 { color: #fff; text-align: center; margin: 0 auto 15px; }
.story .block h2 { text-align: left; margin: 1.6em auto 1em;}
.story img { margin: 0 8px; }
.story p, .story ul, .story dt { font-size: 16px; line-height: 1.6em; margin-bottom: 1em; clear: both; }
.story dt { color: #fff; }
.story a { text-decoration: underline; }

.gallery { text-align: center; position: relative; margin: 0 auto;}
.gallery .view { margin: 0 auto;}
.gallery .items { white-space: normal; height: auto; position: relative; padding: 0; margin: 10px 0;}
.gallery .item { float: left;}
.gallery .group { margin: 0 10px; }

.galleryphoto { text-align: center; position: relative; }
.galleryphoto .items { height: auto; position: absolute; top: auto; bottom: 0; right: 0; padding: 0; }
.galleryphoto .item { float: left; line-height: 1px; }

.polaroid { font-family: GoodFootRegular, Helvetica, sans-serif; font-style: normal; background: #fff; color: #222; font-size: 18px; margin: 5px 0 10px;
    -webkit-box-shadow:0 1px 5px #000;
       -moz-box-shadow:0 1px 5px #000;
            box-shadow:0 1px 5px #000;
    }
.polaroid .vignetted { margin: 5px; position: relative; top: 5px; }

.polaroid .vignetted .vignette {
    width: 100%; height: 100%; position: absolute; top: 0; left: 0; z-index: 10;
    -webkit-box-shadow:inset 0 0 5px #000;
       -moz-box-shadow:inset 0 0 5px #000;
            box-shadow:inset 0 0 5px #000;
    }
.polaroid .vignetted img { display: block; z-index: 9; position: relative;}
.polaroid .vignetted div.img { position: relative;}
.polaroid .vignetted div.img img.loading { position: absolute; display: block; top: 37px; left: 64px;}

.polaroid .vignetted ::after { content:"\\00a0"; position:absolute; z-index:1; top:-1px; left:-1px; right:-1px; bottom:-1px; border:2px solid #fff; }

.selected .polaroid {
-webkit-transform: rotate(-1.5deg) scale(1.12);
   -moz-transform: rotate(-1.5deg) scale(1.12);
    -ms-transform: rotate(-1.5deg) scale(1.12);
     -o-transform: rotate(-1.5deg) scale(1.12);
        transform: rotate(-1.5deg) scale(1.12);
    }
.ie8 .selected .group .polaroid a { color: #6ad;}

/* portrait, mobile */
@media screen and (max-device-width: 480px) {
  h1 { font-size: 30px; line-height: 1em; }
  #c { font-size: 10px;}
}
             """ ]
    , div [ id "head" ]
      [ h1 []
        [ a [ href "#home", onClick Latest ]
          [ text "million.nl" ]
        ]
      ]
    , button [ onClick Previous ]
      [ text "previous" ]
    , button [ onClick Next ]
      [ text "next" ]
    , div []
      [ text model.message ]
    , div [ id "content", class "home" ]
      [ div [ class "view" ]
        [ viewEntries model ]
      ]
    , div [ id "c" ]
      [ text "© 2001-"
      , span [ class "thisyear" ]
        [ text "2017" ]
      , text " Marc Boeren. All rights reserved."
      ]
    ]



viewEntries : Model -> Html Msg
viewEntries model =
  div [ class "items", unselectable "on"]
    (List.indexedMap (\index entry -> viewEntry entry index (index == model.activeEntry)) model.entries)

viewEntry : Entry -> Int -> Bool -> Html Msg
viewEntry entry index isActive =
  let
    itemClass =
      if isActive then "item selected"
      else "item"
    entryView =
      case entry.entrytype of
         Gallery -> viewGalleryEntry
         Story -> viewStoryEntry
  in
    div [ class itemClass, onClick (Select index), unselectable "on" ]
    [ entryView entry
    ]


zeroPadded : Int -> Int -> String
zeroPadded number length =
  (padLeft length '0' (toString number))

-- 2000-03-30 17:45:00
formatDateTime : DateTime -> String
formatDateTime timestamp =
  (toString (DateTime.year timestamp)) ++ "-" ++
  (zeroPadded (DateTime.month timestamp) 2) ++ "-" ++
  (zeroPadded (DateTime.day timestamp) 2) ++ " " ++
  (zeroPadded (DateTime.hour timestamp) 2) ++ ":" ++
  (zeroPadded (DateTime.minute timestamp) 2) ++ ":" ++
  (zeroPadded (DateTime.second timestamp) 2)

-- 30 - 3 - 2000
formatDate : DateTime -> String
formatDate timestamp =
  (zeroPadded (DateTime.day timestamp) 2) ++ " - " ++ (zeroPadded (DateTime.month timestamp) 2) ++ " - " ++ (toString (DateTime.year timestamp))

viewGalleryEntry : Entry -> Html Msg
viewGalleryEntry entry =
  let
    countText =
      if entry.count <= 1 then "(" ++ (toString entry.count) ++ " photo)"
      else "(" ++ (toString entry.count) ++ " photos)"
  in
    div [ class "group", unselectable "on" ]
    [ img [ class "bck", src "http://www.million.nl/css/erased.png", alt "", unselectable "on" ] []
    , div [ class "polaroid", unselectable "on" ]
      [ div [ class "vignetted", unselectable "on" ]
        [ div [ class "vignette", unselectable "on" ] []
        , img [ width 160, height 106, src ("http://www.million.nl/galleries/" ++ entry.slug ++ "/index@2x.jpg"), alt "", unselectable "on" ] []
        ]
      , a [ unselectable "on", property "innerHTML" (string entry.title) ] []
      ]
    , time [ datetime (formatDateTime entry.timestamp), unselectable "on" ]
      [ text (formatDate entry.timestamp)
      ]
    , p [ unselectable "on", property "innerHTML" (string entry.text) ] []
    , p [ unselectable "on" ]
      [ text countText ]
    ]

viewStoryEntry : Entry -> Html Msg
viewStoryEntry entry =
    div [ class "group", unselectable "on" ]
    [ img [ class "bck", src "http://www.million.nl/css/erased.png", alt "", unselectable "on" ] []
    , time [ datetime (formatDateTime entry.timestamp), unselectable "on" ]
      [ text (formatDate entry.timestamp)
      ]
    , div [ class "polaroid", unselectable "on" ]
      [ div [ class "vignetted", unselectable "on" ]
        [ div [ class "vignette", unselectable "on" ] []
        , img [ width 160, height 106, src ("http://www.million.nl/stories/" ++ entry.slug ++ "/index@2x.jpg"), alt "", unselectable "on" ] []
        ]
      , a [ unselectable "on", property "innerHTML" (string entry.title) ] []
      ]
    , p [ unselectable "on", property "innerHTML" (string entry.text) ] []
    , p [ unselectable "on" ]
      [ text "(a story)" ]
    ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- HTTP


getEntries : Cmd Msg
getEntries =
  let
    url =
      "http://www.million.nl/json/index.json"
  in
    Http.send NewEntries (Http.get url decodeEntriesJson)

timestampDecoder : Decode.Decoder DateTime
timestampDecoder = Decode.succeed (DateTime.fromTuple (2003, 04, 30, 17, 54, 00, 0))

entrytypeDecoder : Decode.Decoder EntryType
entrytypeDecoder =
    Decode.string
        |> Decode.andThen (\entrytype ->
           case entrytype of
                "gallery" ->
                    Decode.succeed Gallery
                "story" ->
                    Decode.succeed Story
                _ ->
                    Decode.succeed Gallery
        )

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
