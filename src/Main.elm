port module Main exposing (Model, Msg(..), init, main, update, view)

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
    , config : DisplaySettings
    }


type alias DisplaySettings =
    { showYear : Bool
    , showQuater : Bool
    , showTermin : Bool
    , showMonth : Bool
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
    , config : DisplaySettings
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
        decoderDisplay =
            Json.map4 DisplaySettings
                (Json.field "showYear" Json.bool
                    |> Json.maybe
                    |> Json.map (Maybe.withDefault False)
                )
                (Json.field "showQuater" Json.bool
                    |> Json.maybe
                    |> Json.map (Maybe.withDefault False)
                )
                (Json.field "showTermin" Json.bool
                    |> Json.maybe
                    |> Json.map (Maybe.withDefault False)
                )
                (Json.field "showMonth" Json.bool
                    |> Json.maybe
                    |> Json.map (Maybe.withDefault False)
                )

        decoder =
            Json.map3 InitFlags
                (Json.field "from" (Json.int |> Json.map Time.millisToPosix))
                (Json.field "to" (Json.int |> Json.map Time.millisToPosix))
                (Json.field "config" decoderDisplay
                    |> Json.maybe
                    |> Json.map (Maybe.withDefault (DisplaySettings True True True True))
                )
    in
    case Json.decodeValue decoder input of
        Ok flags ->
            ( { picked = Nothing
              , first_date = flags.from
              , last_date = flags.to
              , config = flags.config
              }
            , Cmd.none
            )

        Err error ->
            ( { picked = Nothing
              , first_date = Time.millisToPosix 1564617600000
              , last_date = Time.millisToPosix 1564617600000
              , config = DisplaySettings True True True True
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


rowTypeYear : RowType -> Int
rowTypeYear row =
    case row of
        MyYear pp ->
            pp.year

        MyQuater pp ->
            pp.year

        MyTermin pp ->
            pp.year

        MyPeriod pp ->
            pp.year


getEndDate : RowType -> Time.Posix
getEndDate row =
    let
        month =
            case row of
                MyYear pp ->
                    12

                MyQuater pp ->
                    pp.quater * 3

                MyTermin pp ->
                    pp.termin * 2

                MyPeriod pp ->
                    pp.month

        basefx =
            toDate (rowTypeYear row)
    in
    case month of
        12 ->
            basefx month 31

        _ ->
            basefx (month + 1) 0


toDate : Int -> Int -> Int -> Time.Posix
toDate year month day =
    let
        month_str =
            month |> String.fromInt |> (++) "0" |> String.right 2

        day_str =
            day |> String.fromInt |> (++) "0" |> String.right 2

        string =
            String.fromInt year
                ++ "-"
                ++ month_str
                ++ "-"
                ++ day_str
                ++ "T00:00:00.000Z"
    in
    string
        |> Iso8601.toTime
        |> Result.withDefault (Time.millisToPosix 100000)


toStringFromTime : Time.Posix -> String
toStringFromTime time =
    (Time.toYear utc time |> String.fromInt)
        ++ "-"
        ++ (Time.toMonth utc time |> toNumishMonth |> String.fromInt |> (++) "0" |> String.right 2)
        ++ "-"
        ++ (Time.toDay utc time |> String.fromInt |> (++) "0" |> String.right 2)


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



---- UPDATE ----


type Msg
    = NoOp
    | Pick RowType
    | UnPick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick newpicked ->
            ( { model | picked = Just newpicked }
            , emitSelected
                { start = newpicked |> getStartDate |> toStringFromTime
                , end = newpicked |> getEndDate |> toStringFromTime
                }
            )

        NoOp ->
            ( model, Cmd.none )

        UnPick ->
            ( { model | picked = Nothing }, emitUnpicked () )


port emitSelected : { end : String, start : String } -> Cmd msg


port emitUnpicked : () -> Cmd msg



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick UnPick ] [ text "unpick" ]
        , div [ class "calenders" ]
            (List.range
                (model.first_date |> Time.toYear utc)
                (model.last_date |> Time.toYear utc)
                |> List.map (printTheShit model)
            )
        ]


printTheShit : Model -> Int -> Html Msg
printTheShit model year =
    let
        htmlYear =
            if model.config.showYear then
                printYear model.picked year |> Just

            else
                Nothing

        htmlQuater =
            if model.config.showQuater then
                printQuaters (calQuaterInPeriod model.first_date model.last_date) model.picked year |> Just

            else
                Nothing

        htmlTermin =
            if model.config.showTermin then
                printTermins (calTerminInPeriod model.first_date model.last_date) model.picked year |> Just

            else
                Nothing

        htmlMonth =
            if model.config.showMonth then
                printMonths (calPeriodsInPeriod model.first_date model.last_date) model.picked year |> Just

            else
                Nothing

        prints =
            [ htmlYear, htmlQuater, htmlTermin, htmlMonth ]
    in
    div [ class "calender" ]
        (List.filterMap (\x -> x) prints)


printYear : Maybe RowType -> Int -> Html Msg
printYear selected year =
    div [ class "calender-rows" ]
        [ viewPrintBlock
            (MyYear { year = year })
            selected
            True
            (String.fromInt year)
        ]


printQuaters : List CalQuater -> Maybe RowType -> Int -> Html Msg
printQuaters list selected year =
    div [ class "calender-rows" ]
        (List.range 1 4
            |> List.map
                (\int ->
                    viewPrintBlock
                        (MyQuater { year = year, quater = int })
                        selected
                        (List.any (\x -> x.year == year && x.quater == int) list)
                        ("q" ++ String.fromInt int)
                )
        )


printTermins : List CalTermin -> Maybe RowType -> Int -> Html Msg
printTermins list selected year =
    div [ class "calender-rows" ]
        (List.range 1 6
            |> List.map
                (\int ->
                    viewPrintBlock
                        (MyTermin { year = year, termin = int })
                        selected
                        (List.any (\x -> x.year == year && x.termin == int) list)
                        ("t" ++ String.fromInt int)
                )
        )


printMonths : List CalPeriod -> Maybe RowType -> Int -> Html Msg
printMonths list selected year =
    div [ class "calender-rows" ]
        (List.range 1 12
            |> List.map
                (\mnd ->
                    viewPrintBlock
                        (MyPeriod { year = year, month = mnd })
                        selected
                        (List.any (\x -> x.year == year && x.month == mnd) list)
                        ("m" ++ String.fromInt mnd)
                )
        )


viewPrintBlock : RowType -> Maybe RowType -> Bool -> String -> Html Msg
viewPrintBlock row selected enabled teext =
    case enabled of
        True ->
            case selected of
                Just picked ->
                    if picked == row then
                        div [ class "calender-rows-unit" ]
                            [ button [ onClick (Pick row), class "elm-period-picker-selected" ] [ text teext ]
                            ]

                    else
                        div [ class "calender-rows-unit" ]
                            [ button [ onClick (Pick row) ] [ text teext ]
                            ]

                Nothing ->
                    div [ class "calender-rows-unit" ]
                        [ button [ onClick (Pick row) ] [ text teext ]
                        ]

        False ->
            div [ class "calender-rows-unit" ]
                [ button [ disabled True ] [ text teext ]
                ]



---- PROGRAM ----


main : Program Json.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
