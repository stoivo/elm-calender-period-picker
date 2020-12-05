port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Dom exposing (Viewport)
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (class, disabled, id, src)
import Html.Events exposing (onClick)
import Iso8601
import Json.Decode as Json
import List exposing ((::))
import Task
import Time exposing (utc)
import Time.Extra



---- MODEL ----


type alias InitFlags =
    { from : Time.Posix
    , to : Time.Posix
    , config : DisplaySettings
    }


type alias DisplaySettings =
    { showUnpick : Bool
    , showYear : Bool
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


type alias CalMonth =
    { year : Int
    , month : Int
    }


type RowType
    = MyYear CalYear
    | MyQuater CalQuater
    | MyTermin CalTermin
    | MyPeriod CalMonth


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


calMonthsInPeriod : Time.Posix -> Time.Posix -> List CalMonth
calMonthsInPeriod first last =
    calMonthsInPeriodRec first last []


calMonthsInPeriodRec : Time.Posix -> Time.Posix -> List CalMonth -> List CalMonth
calMonthsInPeriodRec first last agg =
    if Time.posixToMillis last < Time.posixToMillis first then
        agg

    else
        calMonthsInPeriodRec
            (Time.Extra.add Time.Extra.Month 1 utc first)
            last
            ({ year = Time.toYear utc first, month = Time.toMonth utc first |> toNumishMonth } :: agg)


initDecoder : Json.Decoder InitFlags
initDecoder =
    let
        decoderDisplay =
            Json.map5 DisplaySettings
                (Json.field "showUnpick" Json.bool
                    |> Json.maybe
                    |> Json.map (Maybe.withDefault False)
                )
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
    in
    Json.map3 InitFlags
        (Json.field "from" (Json.int |> Json.map Time.millisToPosix))
        (Json.field "to" (Json.int |> Json.map Time.millisToPosix))
        (Json.field "config" decoderDisplay
            |> Json.maybe
            |> Json.map (Maybe.withDefault (DisplaySettings True True True True True))
        )


init : Json.Value -> ( Model, Cmd Msg )
init input =
    case Json.decodeValue initDecoder input of
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
              , config = DisplaySettings True True True True True
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

        year =
            toDate (rowTypeYear row)
    in
    case month of
        12 ->
            year month 31

        _ ->
            year (month + 1) 0


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
    ""
        ++ (Time.toYear utc time |> String.fromInt)
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
    | ScrollY Int


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

        ScrollY pixels ->
            -- https://ellie-app.com/55LNyTg8f3ka1
            ( model, jumpToBottom "calenders" pixels )

        NoOp ->
            ( model, Cmd.none )

        UnPick ->
            ( { model | picked = Nothing }, emitUnpicked () )


jumpToBottom : String -> Int -> Cmd Msg
jumpToBottom id pixels =
    Browser.Dom.getViewportOf id
        |> Task.andThen (\info -> Browser.Dom.setViewportOf id ((+) (toFloat pixels) info.viewport.x) 0)
        |> Task.attempt (\_ -> NoOp)


port emitSelected : { end : String, start : String } -> Cmd msg


port emitUnpicked : () -> Cmd msg



---- VIEW ----


type alias ScrollEventMini =
    { deltaY : Int
    , deltaX : Int
    }


onWheelVerticalScroll : Html.Attribute Msg
onWheelVerticalScroll =
    Html.Events.custom "wheel"
        (Json.map2 ScrollEventMini
            (Json.field "deltaY" Json.int)
            (Json.field "deltaX" Json.int)
            |> Json.map
                (\event ->
                    { message = ScrollY ((event.deltaX + event.deltaY) * 4)
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
        )


view : Model -> Html Msg
view model =
    let
        calenders =
            div
                [ id "calenders"
                , class "calenders"
                , onWheelVerticalScroll
                ]
                (List.range
                    (model.first_date |> Time.toYear utc)
                    (model.last_date |> Time.toYear utc)
                    |> List.map (viewCalender model)
                )
    in
    if model.config.showUnpick then
        div [ class "structure-holder" ]
            [ button [ onClick UnPick ] [ text "unpick" ]
            , calenders
            , button [ id "button-to-go-left", onClick (ScrollY -100) ] [ text "⏪" ]
            , button [ id "button-to-go-right", onClick (ScrollY 100) ] [ text "⏩" ]
            ]

    else
        div [] [ calenders ]


viewCalender : Model -> Int -> Html Msg
viewCalender model year =
    let
        htmlYear =
            if model.config.showYear then
                Just <| viewYear model.picked year

            else
                Nothing

        htmlQuater =
            if model.config.showQuater then
                Just <| viewQuaters (calQuaterInPeriod model.first_date model.last_date) model.picked year

            else
                Nothing

        htmlTermin =
            if model.config.showTermin then
                Just <| viewTermins (calTerminInPeriod model.first_date model.last_date) model.picked year

            else
                Nothing

        htmlMonth =
            if model.config.showMonth then
                Just <| viewMonths (calMonthsInPeriod model.first_date model.last_date) model.picked year

            else
                Nothing

        prints =
            [ htmlYear, htmlQuater, htmlTermin, htmlMonth ]
    in
    div [ class "calender" ]
        (List.filterMap (\x -> x) prints)


viewYear : Maybe RowType -> Int -> Html Msg
viewYear selected year =
    div [ class "calender-rows" ]
        [ viewPrintBlock
            (MyYear { year = year })
            selected
            True
            (String.fromInt year)
        ]


viewQuaters : List CalQuater -> Maybe RowType -> Int -> Html Msg
viewQuaters list selected year =
    let
        view_ =
            \int ->
                viewPrintBlock
                    (MyQuater { year = year, quater = int })
                    selected
                    (List.any (\x -> x.year == year && x.quater == int) list)
                    ("Q " ++ String.fromInt int)
    in
    div [ class "calender-rows" ]
        (List.map view_ (List.range 1 4))


viewTermins : List CalTermin -> Maybe RowType -> Int -> Html Msg
viewTermins list selected year =
    let
        view_ =
            \int ->
                viewPrintBlock
                    (MyTermin { year = year, termin = int })
                    selected
                    (List.any (\x -> x.year == year && x.termin == int) list)
                    ("T " ++ String.fromInt int)
    in
    div [ class "calender-rows" ]
        (List.map view_ (List.range 1 6))


viewMonths : List CalMonth -> Maybe RowType -> Int -> Html Msg
viewMonths list selected year =
    let
        view_ =
            \month ->
                viewPrintBlock
                    (MyPeriod { year = year, month = month })
                    selected
                    (List.any (\x -> x.year == year && x.month == month) list)
                    (monthToStr month)
    in
    div [ class "calender-rows" ]
        (List.map view_ (List.range 1 12))


monthToStr : Int -> String
monthToStr month =
    case month of
        1 ->
            "Jan"

        2 ->
            "Feb"

        3 ->
            "Mar"

        4 ->
            "Apr"

        5 ->
            "Mai"

        6 ->
            "Jun"

        7 ->
            "Jul"

        8 ->
            "Aug"

        9 ->
            "Sep"

        10 ->
            "Okt"

        11 ->
            "Nov"

        12 ->
            "Dev"

        _ ->
            "-"


viewPrintBlock : RowType -> Maybe RowType -> Bool -> String -> Html Msg
viewPrintBlock row selected enabled body =
    case enabled of
        True ->
            if selected == Just row then
                div [ class "calender-rows-unit" ]
                    [ button [ onClick (Pick row), class "elm-period-picker-selected" ] [ text body ]
                    ]

            else
                div [ class "calender-rows-unit" ]
                    [ button [ onClick (Pick row) ] [ text body ]
                    ]

        False ->
            div [ class "calender-rows-unit" ]
                [ button [ disabled True ] [ text body ]
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
