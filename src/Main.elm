module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List exposing ((::))



---- MODEL ----


type alias CalYear =
    { year : Int
    }


type alias CalQuater =
    { year : Int
    , quater : Int
    }


type alias CalTermin =
    { year : Int
    , termin : Int
    }


type alias CalPeriod =
    { year : Int
    , month : Int
    }


type RowType
    = MyYear CalYear
    | MyQuater CalQuater
    | MyTermin CalTermin
    | MyPeriod CalPeriod


type alias Model =
    { picked : Maybe RowType
    }


init : ( Model, Cmd Msg )
init =
    ( { picked = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp



--| AddYear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --AddYear ->
        --    ( { model | pickedHistory = ( Year { year = 2019 }, Year { year = 2019 } ) :: model.pickedHistory }, Cmd.none )
        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ displayLastClicked model.picked
        , div [ class "superholder" ]
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


displayLastClicked : Maybe RowType -> Html Msg
displayLastClicked row =
    case row of
        Just (MyYear pp) ->
            p [] [ pp.year |> String.fromInt |> text ]

        Just (MyQuater pp) ->
            p [] [ text (String.fromInt pp.year ++ " Q" ++ String.fromInt pp.quater) ]

        Just (MyTermin pp) ->
            p [] [ text (String.fromInt pp.year ++ " T" ++ String.fromInt pp.termin) ]

        Just (MyPeriod pp) ->
            p [] [ text (String.fromInt pp.year ++ "-" ++ String.fromInt pp.month) ]

        Nothing ->
            p [] [ text "Nothing" ]



--addYear : Model -> Html Msg
--addYear model =
--    div []
--        [ button [ onClick AddYear ] [ text "Year" ]
--        ]
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
