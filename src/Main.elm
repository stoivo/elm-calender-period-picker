module Main exposing (Model, Msg(..), init, main, printMonths, printQuaters, printTermins, printYear, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "superholder" ]
            [ div [ class "wit" ]
                [ div [ class "holder" ]
                    [ printYear "2018"
                    , printQuaters
                    , printTermins
                    , printMonths
                    ]
                , div [ class "holder" ]
                    [ printYear "2019"
                    , printQuaters
                    , printTermins
                    , printMonths
                    ]
                ]
            ]
        ]


printYear : String -> Html Msg
printYear year =
    div [ class "row" ]
        [ div [ class "item" ] [ text year ] ]


printQuaters : Html Msg
printQuaters =
    div [ class "row" ]
        [ div [ class "item" ] [ text "Q1" ]
        , div [ class "item" ] [ text "Q2" ]
        , div [ class "item" ] [ text "Q3" ]
        , div [ class "item" ] [ text "Q4" ]
        ]


printTermins : Html Msg
printTermins =
    div [ class "row" ]
        [ div [ class "item" ] [ text "jan-feb" ]
        , div [ class "item" ] [ text "mar-apr" ]
        , div [ class "item" ] [ text "mai-jun" ]
        , div [ class "item" ] [ text "jul-aug" ]
        , div [ class "item" ] [ text "sep-okt" ]
        , div [ class "item" ] [ text "nov-des" ]
        ]


printMonths : Html Msg
printMonths =
    div [ class "row" ]
        [ div [ class "item" ] [ text "jan" ]
        , div [ class "item" ] [ text "feb" ]
        , div [ class "item" ] [ text "mar" ]
        , div [ class "item" ] [ text "apr" ]
        , div [ class "item" ] [ text "mai" ]
        , div [ class "item" ] [ text "jun" ]
        , div [ class "item" ] [ text "jul" ]
        , div [ class "item" ] [ text "aug" ]
        , div [ class "item" ] [ text "sep" ]
        , div [ class "item" ] [ text "okt" ]
        , div [ class "item" ] [ text "nov" ]
        , div [ class "item" ] [ text "des" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
