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
    | Pick RowType
    | UnPick



--| AddYear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick newpicked ->
            ( { model | picked = Just newpicked }, Cmd.none )

        --AddYear ->
        --    ( { model | pickedHistory = ( Year { year = 2019 }, Year { year = 2019 } ) :: model.pickedHistory }, Cmd.none )
        NoOp ->
            ( model, Cmd.none )

        UnPick ->
            ( model, Cmd.none )



--( { model | picked = Nothing }, Cmd.none )
---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ displayLastClicked model.picked
        , button [ onClick UnPick ] [ text "unpick" ]
        , div [ class "superholder" ]
            [ div [ class "wit" ]
                [ div [ class "holder" ]
                    [ printYear 2018
                    , printQuaters 2018
                    , printTermins 2018
                    , printMonths 2018
                    ]
                , div [ class "holder" ]
                    [ printYear 2019
                    , printQuaters 2019
                    , printTermins 2019
                    , printMonths 2019
                    ]
                , div [ class "holder" ]
                    [ printYear 2020
                    , printQuaters 2020
                    , printTermins 2020
                    , printMonths 2020
                    ]
                ]
            ]
        ]


printYear : Int -> Html Msg
printYear year =
    div [ class "row" ]
        [ div
            [ class "item", onClick (Pick (MyYear { year = year })) ]
            [ String.fromInt year |> text ]
        ]


printQuaters : Int -> Html Msg
printQuaters year =
    div [ class "row" ]
        [ div [ class "item", onClick (Pick (MyQuater { year = year, quater = 1 })) ] [ text "Q1" ]
        , div [ class "item", onClick (Pick (MyQuater { year = year, quater = 2 })) ] [ text "Q2" ]
        , div [ class "item", onClick (Pick (MyQuater { year = year, quater = 3 })) ] [ text "Q3" ]
        , div [ class "item", onClick (Pick (MyQuater { year = year, quater = 4 })) ] [ text "Q4" ]
        ]


printTermins : Int -> Html Msg
printTermins year =
    div [ class "row" ]
        [ div [ class "item", onClick (Pick (MyTermin { year = year, termin = 1 })) ] [ text "jan-feb" ]
        , div [ class "item", onClick (Pick (MyTermin { year = year, termin = 2 })) ] [ text "mar-apr" ]
        , div [ class "item", onClick (Pick (MyTermin { year = year, termin = 3 })) ] [ text "mai-jun" ]
        , div [ class "item", onClick (Pick (MyTermin { year = year, termin = 4 })) ] [ text "jul-aug" ]
        , div [ class "item", onClick (Pick (MyTermin { year = year, termin = 5 })) ] [ text "sep-okt" ]
        , div [ class "item", onClick (Pick (MyTermin { year = year, termin = 6 })) ] [ text "nov-des" ]
        ]


printMonths : Int -> Html Msg
printMonths year =
    div [ class "row" ]
        [ div [ class "item", onClick (Pick (MyPeriod { year = year, month = 1 })) ] [ text "jan" ]
        , div [ class "item", onClick (Pick (MyPeriod { year = year, month = 2 })) ] [ text "feb" ]
        , div [ class "item", onClick (Pick (MyPeriod { year = year, month = 3 })) ] [ text "mar" ]
        , div [ class "item", onClick (Pick (MyPeriod { year = year, month = 4 })) ] [ text "apr" ]
        , div [ class "item", onClick (Pick (MyPeriod { year = year, month = 5 })) ] [ text "mai" ]
        , div [ class "item", onClick (Pick (MyPeriod { year = year, month = 6 })) ] [ text "jun" ]
        , div [ class "item", onClick (Pick (MyPeriod { year = year, month = 7 })) ] [ text "jul" ]
        , div [ class "item", onClick (Pick (MyPeriod { year = year, month = 8 })) ] [ text "aug" ]
        , div [ class "item", onClick (Pick (MyPeriod { year = year, month = 9 })) ] [ text "sep" ]
        , div [ class "item", onClick (Pick (MyPeriod { year = year, month = 10 })) ] [ text "okt" ]
        , div [ class "item", onClick (Pick (MyPeriod { year = year, month = 11 })) ] [ text "nov" ]
        , div [ class "item", onClick (Pick (MyPeriod { year = year, month = 12 })) ] [ text "des" ]
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
