module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (class, disabled, src)
import Html.Events exposing (onClick)
import Iso8601
import Json.Decode as Json
import List exposing ((::))
import Time exposing (utc)
import Time.Extra



---- MODEL ----


type alias InitFlags =
    { from : Time.Posix
    , to : Time.Posix
    }


type alias CalYear =
    { year : Int }


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


init : Json.Value -> ( Model, Cmd Msg )
init input =
    let
        decoder =
            Json.map2 InitFlags
                (Json.field "from" (Json.int |> Json.map Time.millisToPosix))
                (Json.field "to" (Json.int |> Json.map Time.millisToPosix))
    in
    case Json.decodeValue decoder input of
        Ok flags ->
            ( { picked = Nothing
              , first_date = flags.from
              , last_date = flags.to
              }
            , Cmd.none
            )

        Err error ->
            ( { picked = Nothing
              , first_date = Time.millisToPosix 1564617600000
              , last_date = Time.millisToPosix 1564617600000
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



---- UPDATE ----


type Msg
    = NoOp
    | Pick RowType
    | UnPick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick newpicked ->
            ( { model | picked = Just newpicked }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        UnPick ->
            ( { model | picked = Nothing }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ mRowView model.picked
        , mRowBeginningView model.picked
        , mRowEndView model.picked
        , button [ onClick UnPick ] [ text "unpick" ]
        , div [ class "calenders" ]
            (List.range
                (model.first_date |> Time.toYear utc)
                (model.last_date |> Time.toYear utc)
                |> List.map (printTheShit model)
            )
        ]


printTheShit : Model -> Int -> Html Msg
printTheShit model year =
    div [ class "calender" ]
        [ printStartAndEnd model
        , printYear year
        , printQuaters (calQuaterInPeriod model.first_date model.last_date) year
        , printTermins (calTerminInPeriod model.first_date model.last_date) year
        , printMonths (calPeriodsInPeriod model.first_date model.last_date) year
        ]


printStartAndEnd : Model -> Html Msg
printStartAndEnd model =
    div [ class "calender-rows" ]
        [ div
            [ class "calender-rows-unit" ]
            [ (toStringFromTime model.first_date ++ " to " ++ toStringFromTime model.last_date) |> text ]
        ]


printYear : Int -> Html Msg
printYear year =
    div [ class "calender-rows" ]
        [ viewPrintBlock
            (MyYear { year = year })
            True
            (String.fromInt year)
        ]


printQuaters : List CalQuater -> Int -> Html Msg
printQuaters list year =
    div [ class "calender-rows" ]
        (List.range 1 4
            |> List.map
                (\int ->
                    viewPrintBlock
                        (MyQuater { year = year, quater = int })
                        (List.any (\x -> x.year == year && x.quater == int) list)
                        ("q" ++ String.fromInt int)
                )
        )


printTermins : List CalTermin -> Int -> Html Msg
printTermins list year =
    div [ class "calender-rows" ]
        (List.range 1 6
            |> List.map
                (\int ->
                    viewPrintBlock
                        (MyTermin { year = year, termin = int })
                        (List.any (\x -> x.year == year && x.termin == int) list)
                        ("t" ++ String.fromInt int)
                )
        )


printMonths : List CalPeriod -> Int -> Html Msg
printMonths list year =
    div [ class "calender-rows" ]
        (List.range 1 12
            |> List.map
                (\mnd ->
                    viewPrintBlock
                        (MyPeriod { year = year, month = mnd })
                        (List.any (\x -> x.year == year && x.month == mnd) list)
                        ("m" ++ String.fromInt mnd)
                )
        )


viewPrintBlock : RowType -> Bool -> String -> Html Msg
viewPrintBlock row enabled teext =
    case enabled of
        True ->
            div [ class "calender-rows-unit" ]
                [ button [ onClick (Pick row) ] [ text teext ]
                ]

        False ->
            div [ class "calender-rows-unit" ]
                [ button [ disabled True ] [ text teext ]
                ]


mRowView : Maybe RowType -> Html Msg
mRowView row =
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


mRowBeginningView : Maybe RowType -> Html Msg
mRowBeginningView row =
    case row of
        Just x ->
            p [] [ getStartDate x |> toStringFromTime |> text ]

        Nothing ->
            p [] [ text "Nothing" ]


mRowEndView : Maybe RowType -> Html Msg
mRowEndView row =
    case row of
        Just x ->
            p [] [ getEndDate x |> toStringFromTime |> text ]

        Nothing ->
            p [] [ text "Nothing" ]



---- PROGRAM ----


main : Program Json.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
