module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Iso8601
import List exposing ((::))
import Time exposing (utc)
import Time.Extra



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
    , first_date : Time.Posix
    , last_date : Time.Posix
    }


calQuaterInPeriod : Time.Posix -> Time.Posix -> List CalQuater
calQuaterInPeriod first last =
    calQuaterInPeriodRec first last []


calQuaterInPeriodRec : Time.Posix -> Time.Posix -> List CalQuater -> List CalQuater
calQuaterInPeriodRec first last agg =
    if Time.posixToMillis last < Time.posixToMillis first then
        agg

    else
        let
            first_month =
                Time.toMonth utc first |> toNumishMonth

            highest_month =
                if (first_month |> modBy 3) == 0 then
                    first_month

                else
                    first_month + (3 - (first_month |> modBy 3))

            quater =
                highest_month // 3
        in
        calQuaterInPeriodRec
            (Time.Extra.add Time.Extra.Month 3 utc first)
            last
            ({ year = Time.toYear utc first
             , quater = quater
             }
                :: agg
            )


calTerminInPeriod : Time.Posix -> Time.Posix -> List CalTermin
calTerminInPeriod first last =
    calTerminInPeriodRec first last []


calTerminInPeriodRec : Time.Posix -> Time.Posix -> List CalTermin -> List CalTermin
calTerminInPeriodRec first last agg =
    if Time.posixToMillis last < Time.posixToMillis first then
        agg

    else
        let
            highest_month =
                if (Time.toMonth utc first |> toNumishMonth |> modBy 2) == 0 then
                    Time.toMonth utc first |> toNumishMonth

                else
                    (Time.toMonth utc first |> toNumishMonth) + 1

            termine =
                highest_month // 2
        in
        calTerminInPeriodRec
            (Time.Extra.add Time.Extra.Month 2 utc first)
            last
            ({ year = Time.toYear utc first
             , termin = termine
             }
                :: agg
            )


calPeriodsInPeriod : Time.Posix -> Time.Posix -> List CalPeriod
calPeriodsInPeriod first last =
    calPeriodsInPeriodRec first last []


calPeriodsInPeriodRec : Time.Posix -> Time.Posix -> List CalPeriod -> List CalPeriod
calPeriodsInPeriodRec first last agg =
    if Time.posixToMillis last < Time.posixToMillis first then
        agg

    else
        calPeriodsInPeriodRec
            (Time.Extra.add Time.Extra.Month 1 utc first)
            last
            ({ year = Time.toYear utc first, month = Time.toMonth utc first |> toNumishMonth } :: agg)


init : ( Int, Int ) -> ( Model, Cmd Msg )
init currentTime =
    let
        first_date =
            currentTime
                |> Tuple.first
                |> Time.millisToPosix

        last_date =
            currentTime
                |> Tuple.second
                |> Time.millisToPosix
    in
    ( { picked = Nothing
      , first_date = first_date
      , last_date = last_date
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


toNumishMonth : Time.Month -> Int
toNumishMonth month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


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
            [ div
                [ class "wit" ]
                (List.range (model.first_date |> Time.toYear utc) (model.last_date |> Time.toYear utc)
                    |> List.map (\year -> printTheShit model year)
                )
            ]
        ]


printTheShit : Model -> Int -> Html Msg
printTheShit model year =
    div [ class "holder" ]
        [ printStartAndEnd model
        , printYear year
        , printQuaters (calQuaterInPeriod model.first_date model.last_date) year
        , printTermins (calTerminInPeriod model.first_date model.last_date) year
        , printMonths (calPeriodsInPeriod model.first_date model.last_date) year
        ]


printStartAndEnd : Model -> Html Msg
printStartAndEnd model =
    div [ class "row" ]
        [ div
            [ class "item item-border" ]
            [ (toStringFromTime model.first_date ++ " to " ++ toStringFromTime model.last_date) |> text ]
        ]


printYear : Int -> Html Msg
printYear year =
    div [ class "row" ]
        [ div
            [ class "item item-border", onClick (Pick (MyYear { year = year })) ]
            [ String.fromInt year |> text ]
        ]


printQuaters : List CalQuater -> Int -> Html Msg
printQuaters list year =
    let
        q1 =
            List.any (\x -> x.year == year && x.quater == 1) list

        q2 =
            List.any (\x -> x.year == year && x.quater == 2) list

        q3 =
            List.any (\x -> x.year == year && x.quater == 3) list

        q4 =
            List.any (\x -> x.year == year && x.quater == 4) list
    in
    div [ class "row" ]
        [ div [ class "item item-border", onClick (Pick (MyQuater { year = year, quater = 1 })) ] [ text (maybeDisabled q1 "Q1") ]
        , div [ class "item item-border", onClick (Pick (MyQuater { year = year, quater = 2 })) ] [ text (maybeDisabled q2 "Q2") ]
        , div [ class "item item-border", onClick (Pick (MyQuater { year = year, quater = 3 })) ] [ text (maybeDisabled q3 "Q3") ]
        , div [ class "item item-border", onClick (Pick (MyQuater { year = year, quater = 4 })) ] [ text (maybeDisabled q4 "Q4") ]
        ]


printTermins : List CalTermin -> Int -> Html Msg
printTermins list year =
    let
        t1 =
            List.any (\x -> x.year == year && x.termin == 1) list

        t2 =
            List.any (\x -> x.year == year && x.termin == 2) list

        t3 =
            List.any (\x -> x.year == year && x.termin == 3) list

        t4 =
            List.any (\x -> x.year == year && x.termin == 4) list

        t5 =
            List.any (\x -> x.year == year && x.termin == 5) list

        t6 =
            List.any (\x -> x.year == year && x.termin == 6) list
    in
    div [ class "row" ]
        [ div [ class "item item-border", onClick (Pick (MyTermin { year = year, termin = 1 })) ] [ text (maybeDisabled t1 "jan-feb") ]
        , div [ class "item item-border", onClick (Pick (MyTermin { year = year, termin = 2 })) ] [ text (maybeDisabled t2 "mar-apr") ]
        , div [ class "item item-border", onClick (Pick (MyTermin { year = year, termin = 3 })) ] [ text (maybeDisabled t3 "mai-jun") ]
        , div [ class "item item-border", onClick (Pick (MyTermin { year = year, termin = 4 })) ] [ text (maybeDisabled t4 "jul-aug") ]
        , div [ class "item item-border", onClick (Pick (MyTermin { year = year, termin = 5 })) ] [ text (maybeDisabled t5 "sep-okt") ]
        , div [ class "item item-border", onClick (Pick (MyTermin { year = year, termin = 6 })) ] [ text (maybeDisabled t6 "nov-des") ]
        ]


printMonths : List CalPeriod -> Int -> Html Msg
printMonths list year =
    let
        m1 =
            List.any (\x -> x.year == year && x.month == 1) list

        m2 =
            List.any (\x -> x.year == year && x.month == 2) list

        m3 =
            List.any (\x -> x.year == year && x.month == 3) list

        m4 =
            List.any (\x -> x.year == year && x.month == 4) list

        m5 =
            List.any (\x -> x.year == year && x.month == 5) list

        m6 =
            List.any (\x -> x.year == year && x.month == 6) list

        m7 =
            List.any (\x -> x.year == year && x.month == 7) list

        m8 =
            List.any (\x -> x.year == year && x.month == 8) list

        m9 =
            List.any (\x -> x.year == year && x.month == 9) list

        m10 =
            List.any (\x -> x.year == year && x.month == 10) list

        m11 =
            List.any (\x -> x.year == year && x.month == 11) list

        m12 =
            List.any (\x -> x.year == year && x.month == 12) list
    in
    div [ class "row" ]
        [ div [ class "item item-border", onClick (Pick (MyPeriod { year = year, month = 1 })) ] [ text (maybeDisabled m1 "jan") ]
        , div [ class "item item-border", onClick (Pick (MyPeriod { year = year, month = 2 })) ] [ text (maybeDisabled m2 "feb") ]
        , div [ class "item item-border", onClick (Pick (MyPeriod { year = year, month = 3 })) ] [ text (maybeDisabled m3 "mar") ]
        , div [ class "item item-border", onClick (Pick (MyPeriod { year = year, month = 4 })) ] [ text (maybeDisabled m4 "apr") ]
        , div [ class "item item-border", onClick (Pick (MyPeriod { year = year, month = 5 })) ] [ text (maybeDisabled m5 "mai") ]
        , div [ class "item item-border", onClick (Pick (MyPeriod { year = year, month = 6 })) ] [ text (maybeDisabled m6 "jun") ]
        , div [ class "item item-border", onClick (Pick (MyPeriod { year = year, month = 7 })) ] [ text (maybeDisabled m7 "jul") ]
        , div [ class "item item-border", onClick (Pick (MyPeriod { year = year, month = 8 })) ] [ text (maybeDisabled m8 "aug") ]
        , div [ class "item item-border", onClick (Pick (MyPeriod { year = year, month = 9 })) ] [ text (maybeDisabled m9 "sep") ]
        , div [ class "item item-border", onClick (Pick (MyPeriod { year = year, month = 10 })) ] [ text (maybeDisabled m10 "okt") ]
        , div [ class "item item-border", onClick (Pick (MyPeriod { year = year, month = 11 })) ] [ text (maybeDisabled m11 "nov") ]
        , div [ class "item item-border", onClick (Pick (MyPeriod { year = year, month = 12 })) ] [ text (maybeDisabled m12 "des") ]
        ]


maybeDisabled : Bool -> String -> String
maybeDisabled enabled string =
    if enabled then
        string

    else
        string ++ " disabled"


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


main : Program ( Int, Int ) Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
