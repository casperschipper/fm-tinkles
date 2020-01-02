port module Main exposing (main)

import Browser
import Collage exposing (circle, filled, group, rectangle, shift, uniform)
import Collage.Layout exposing (at, topLeft)
import Collage.Render exposing (svg)
import Color
import Html exposing (Html, br, div, p, span, text)
import Html.Attributes exposing (style)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { screensize : ScreenSize
    , frames : List Frame
    }


type alias ScreenSize =
    { width : Int
    , height : Int
    }


type Msg
    = FrameReceived E.Value


type alias Frame =
    { f1 : Float
    , f2 : Float
    , f3 : Float
    , f4 : Float
    , f5 : Float
    }


decodeFrame : D.Decoder Frame
decodeFrame =
    D.map5 Frame
        (D.field "f1" D.float)
        (D.field "f2" D.float)
        (D.field "f3" D.float)
        (D.field "f4" D.float)
        (D.field "f5" D.float)


port sendFrame : (E.Value -> msg) -> Sub msg


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


decodeSize =
    D.map2
        ScreenSize
        (D.field "width" D.int)
        (D.field "height" D.int)


init : E.Value -> ( Model, Cmd Msg )
init flags =
    case D.decodeValue decodeSize flags of
        Ok screensize ->
            ( Model screensize [], Cmd.none )

        Err err ->
            let
                _ =
                    Debug.log "it is all lost" err
            in
            ( Model (ScreenSize 0 0) [], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        FrameReceived val ->
            case D.decodeValue decodeFrame val of
                Ok frame ->
                    ( { model | frames = scrollingList frame model.frames }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "json parse failed" err
                    in
                    ( model, Cmd.none )


scrollingList : Frame -> List Frame -> List Frame
scrollingList frame list =
    let
        n =
            List.length list
    in
    if n > 25 then
        List.drop 1 list ++ [ frame ]

    else
        list ++ [ frame ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ sendFrame FrameReceived ]


lijstje : List String -> List (Html Msg)
lijstje strings =
    List.concatMap (\str -> [ span [] [ text str ], br [] [] ]) strings


viewFrame : Frame -> List (Html Msg)
viewFrame frame =
    let
        data =
            [ frame.f1
            , frame.f2
            , frame.f3
            , frame.f4
            , frame.f5
            ]

        stringFloats =
            List.map String.fromFloat data
    in
    lijstje stringFloats


view : Model -> Html Msg
view model =
    let
        circ color ( x, y ) =
            circle 10
                |> filled (uniform color)
                |> shift ( x, y )

        graphAll =
            let
                n =
                    List.length model.frames

                scaleVertical y =
                    (y * toFloat model.screensize.height) - (toFloat model.screensize.height / 2.0)

                frameSlice idx frame =
                    let
                        x =
                            ((toFloat idx / toFloat n) * toFloat model.screensize.width) - (0.5 * toFloat model.screensize.width)

                        y1 =
                            scaleVertical frame.f1

                        y2 =
                            scaleVertical frame.f2

                        y3 =
                            scaleVertical frame.f3

                        y4 =
                            scaleVertical frame.f4

                        y5 =
                            scaleVertical frame.f5
                    in
                    [ circ Color.red ( x, y1 )
                    , circ Color.blue ( x, y2 )
                    , circ Color.green ( x, y3 )
                    , circ Color.yellow ( x, y4 )
                    , circ Color.orange ( x, y5 )
                    ]
            in
            List.concat <| List.indexedMap frameSlice model.frames

        rect =
            rectangle (toFloat model.screensize.width) (toFloat model.screensize.height)
                |> filled (uniform Color.black)

        props =
            String.join " "
                [ "width:"
                , String.fromInt model.screensize.width
                , "height:"
                , String.fromInt model.screensize.height
                ]
    in
    div []
        [ group
            (graphAll
                ++ [ rect
                   ]
            )
            |> svg
        , p [] [ text props ]
        ]
