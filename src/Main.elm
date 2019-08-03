module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Iso8601
import List exposing ((::))
import Time exposing (utc)



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



--- Model helpers ---


getStartDate : RowType -> Time.Posix
getStartDate row =
    case row of
        MyYear pp ->
            toDate pp.year 1 1

        MyQuater pp ->
            toDate pp.year (((pp.quater - 1) * 3) + 1) 1

        MyTermin pp ->
            toDate pp.year (((pp.termin - 1) * 2) + 1) 1

        MyPeriod pp ->
            toDate pp.year pp.month 1


getEndDate : RowType -> Time.Posix
getEndDate row =
    case row of
        MyYear pp ->
            toLatestDateInMonth pp.year 12

        MyQuater pp ->
            toLatestDateInMonth pp.year (pp.quater * 3)

        MyTermin pp ->
            toLatestDateInMonth pp.year (pp.termin * 2)

        MyPeriod pp ->
            toLatestDateInMonth pp.year pp.month


toDate : Int -> Int -> Int -> Time.Posix
toDate year month day =
    let
        month_str =
            if month < 10 then
                String.fromInt month |> (\x -> "0" ++ x)

            else
                String.fromInt month

        day_str =
            if day < 10 then
                String.fromInt day |> (\x -> "0" ++ x)

            else
                String.fromInt day

        string =
            String.fromInt year
                ++ "-"
                ++ month_str
                ++ "-"
                ++ day_str
                ++ "T00:00:00.000Z"
    in
    string
        |> Debug.log "date"
        |> Iso8601.toTime
        |> Result.withDefault (Time.millisToPosix 100000)


toLatestDateInMonth : Int -> Int -> Time.Posix
toLatestDateInMonth year month =
    let
        day =
            case month of
                1 ->
                    31

                2 ->
                    28

                3 ->
                    31

                4 ->
                    30

                5 ->
                    31

                6 ->
                    30

                7 ->
                    31

                8 ->
                    31

                9 ->
                    30

                10 ->
                    31

                11 ->
                    30

                12 ->
                    31

                _ ->
                    5
    in
    toDate year month day


toStringFromTime : Time.Posix -> String
toStringFromTime time =
    (Time.toYear utc time |> String.fromInt)
        ++ "-"
        ++ (Time.toMonth utc time |> toDanishMonth)
        ++ "-"
        ++ (Time.toDay utc time |> String.fromInt)


toDanishMonth : Time.Month -> String
toDanishMonth month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


parseCalQuaterStartDate : CalQuater -> String
parseCalQuaterStartDate cal =
    String.fromInt cal.year ++ "-" ++ String.fromInt (((cal.quater - 1) * 3) + 1) ++ "-01"


parseCalQuaterEndDate : CalQuater -> String
parseCalQuaterEndDate cal =
    String.fromInt cal.year ++ "-" ++ String.fromInt (cal.quater * 3) ++ "-31"


parseCalTerminStartDate : CalTermin -> String
parseCalTerminStartDate cal =
    String.fromInt cal.year ++ "-" ++ String.fromInt (((cal.termin - 1) * 2) + 1) ++ "-01"


parseCalTerminEndDate : CalTermin -> String
parseCalTerminEndDate cal =
    String.fromInt cal.year ++ "-" ++ String.fromInt (cal.termin * 2) ++ "-31"



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
        , ffBeginning model.picked
        , ffend model.picked
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
            p [] [ text (String.fromInt pp.year ++ "-" ++ String.fromInt pp.month ++ "-01") ]

        Nothing ->
            p [] [ text "Nothing" ]


ffBeginning : Maybe RowType -> Html Msg
ffBeginning row =
    case row of
        Just x ->
            p [] [ getStartDate x |> toStringFromTime |> text ]

        Nothing ->
            p [] [ text "Nothing" ]


ffend : Maybe RowType -> Html Msg
ffend row =
    case row of
        Just x ->
            p [] [ getEndDate x |> toStringFromTime |> text ]

        Nothing ->
            p [] [ text "Nothing" ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
