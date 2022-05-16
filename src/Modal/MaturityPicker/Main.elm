module Modal.MaturityPicker.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , update
    , view
    )

import Data.Backdrop exposing (Backdrop)
import Data.Images exposing (Images)
import Data.Maturity exposing (Maturity, posixToMaturity)
import Data.Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Theme exposing (Theme)
import Date exposing (Date)
import DatePicker exposing (ChangeEvent(..))
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , map
        , padding
        , paddingEach
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Modal.Outside as Outside
import Page.Transaction.Button as Button
import Task
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Input as Input
import Utility.Maybe as Maybe
import Utility.ThemeColor as ThemeColor


type Modal
    = Modal
        { date : Maybe Date
        , dateText : String
        , pickerModel : DatePicker.Model
        , hours : String
        , minutes : String
        , posixTime : String
        , pair : Pair
        }


type Msg
    = SetToday Date
    | ChangePicker DatePicker.ChangeEvent
    | InputHour String
    | InputMinute String
    | InputPosix String
    | SelectedMaturity Maturity
    | Exit


type Effect
    = InputMaturity Pool


init : Pair -> ( Modal, Cmd Msg )
init pair =
    ( { date = Nothing
      , dateText = ""
      , pickerModel = DatePicker.init
      , hours = "00"
      , minutes = "00"
      , posixTime = "1654833600"
      , pair = pair
      }
        |> Modal
    , Task.perform SetToday Date.today
    )


update : Msg -> Modal -> ( Maybe Modal, Maybe Effect )
update msg (Modal modal) =
    case msg of
        SetToday today ->
            ( { modal
                | pickerModel =
                    modal.pickerModel
                        |> DatePicker.setToday today
              }
                |> Modal
                |> Just
            , Nothing
            )

        ChangePicker changeEvent ->
            ( case changeEvent of
                DateChanged date ->
                    -- update both date and text
                    { modal
                        | date = Just date
                        , dateText = Date.toIsoString date
                    }
                        |> Modal
                        |> Just

                TextChanged text ->
                    { modal
                        | date =
                            Date.fromIsoString text
                                |> Result.toMaybe
                                |> Maybe.orElse modal.date
                        , dateText = text
                    }
                        |> Modal
                        |> Just

                PickerChanged subMsg ->
                    -- internal stuff changed
                    -- call DatePicker.update
                    { modal
                        | pickerModel =
                            modal.pickerModel
                                |> DatePicker.update subMsg
                    }
                        |> Modal
                        |> Just
            , Nothing
            )

        InputHour hourStr ->
            ( { modal
                | hours =
                    if hourStr |> Input.isInt then
                        hourStr

                    else
                        modal.hours
              }
                |> Modal
                |> Just
            , Nothing
            )

        InputMinute minuteStr ->
            ( { modal
                | minutes =
                    if minuteStr |> Input.isInt then
                        minuteStr

                    else
                        modal.minutes
              }
                |> Modal
                |> Just
            , Nothing
            )

        InputPosix posixStr ->
            ( { modal
                | posixTime =
                    if posixStr |> Input.isInt then
                        posixStr

                    else
                        modal.posixTime
              }
                |> Modal
                |> Just
            , Nothing
            )

        SelectedMaturity maturity ->
            ( modal
                |> Modal
                |> Just
            , Just (InputMaturity { pair = modal.pair, maturity = maturity })
            )

        Exit ->
            ( Nothing, Nothing )


view :
    { model
        | backdrop : Backdrop
        , images : Images
        , theme : Theme
        , time : Posix
    }
    -> Modal
    -> Element Msg
view ({ backdrop, theme, time } as model) ((Modal { date, dateText, hours, minutes, posixTime, pickerModel }) as modal) =
    Outside.view model
        { onClick = Exit
        , modal =
            el
                ([ width <| px 345
                 , height shrink
                 , padding 24
                 , centerX
                 , centerY
                 , Background.color Color.background
                 , Border.rounded 8
                 , theme |> ThemeColor.border |> Border.color
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop theme
                )
                (column
                    [ width fill
                    , height shrink
                    , spacing 16
                    ]
                    [ row
                        [ width fill
                        , height shrink
                        , spacing 16
                        ]
                        [ el
                            [ width shrink
                            , height shrink
                            , centerY
                            , Font.size 18
                            , paddingXY 0 3
                            , Font.color Color.light100
                            ]
                            (text "Input Maturity")
                        , IconButton.exit model Exit
                        ]

                    -- , DatePicker.input
                    --     [ centerX
                    --     , centerY
                    --     , spacing 10
                    --     , Background.color Color.background
                    --     , Border.rounded 8
                    --     , theme |> ThemeColor.border |> Border.color
                    --     , Border.width 1
                    --     , Font.size 14
                    --     , theme |> ThemeColor.textLight |> Font.color
                    --     ]
                    --     { onChange = ChangePicker
                    --     , selected = date
                    --     , text = dateText
                    --     , label =
                    --         Input.labelAbove [ Font.size 14, theme |> ThemeColor.textLight |> Font.color ] <|
                    --             Element.text "Pick A Date"
                    --     , placeholder = Nothing
                    --     , settings = calendarSettings theme
                    --     , model = pickerModel
                    --     }
                    , el
                        [ theme |> ThemeColor.textLight |> Font.color
                        , Font.size 14
                        ]
                        (text "Unix Time")

                    -- , row
                    --     [ width fill
                    --     , height shrink
                    --     , spacing 12
                    --     ]
                    --     [ hourInput model modal
                    --     , minuteInput model modal
                    --     ]
                    , posixInput model modal
                    , if posixTime |> isPosixCorrect time then
                        doneButton theme modal

                      else
                        Button.disabled theme "Done" |> map never
                    ]
                )
        }


hourInput :
    { model
        | backdrop : Backdrop
        , images : Images
        , theme : Theme
    }
    -> Modal
    -> Element Msg
hourInput { theme } (Modal { hours }) =
    row
        [ width fill
        , height fill
        , paddingEach
            { top = 0
            , right = 8
            , bottom = 0
            , left = 0
            }
        , Border.width 1
        , Border.solid
        , (if hours |> isHourCorrect then
            theme |> ThemeColor.textboxBorder

           else
            Color.negative500
          )
            |> Border.color
        , Border.rounded 8
        ]
        [ Input.text
            [ width fill
            , height shrink
            , centerY
            , Background.color Color.none
            , Border.color Color.none
            , Font.bold
            , Font.size 14
            , (if hours |> isHourCorrect then
                theme |> ThemeColor.text

               else
                Color.negative500
              )
                |> Font.color
            ]
            { onChange = InputHour
            , text = hours
            , placeholder =
                Input.placeholder
                    [ (if hours |> isHourCorrect then
                        theme |> ThemeColor.placeholder2

                       else
                        Color.negative500
                      )
                        |> Font.color
                    ]
                    (text "00")
                    |> Just
            , label = Input.labelHidden "Maturity Time (Hours)"
            }
        , el
            [ width shrink
            , height shrink
            , centerY
            , Font.bold
            , Font.size 14
            , (if hours |> isHourCorrect then
                theme |> ThemeColor.text

               else
                Color.negative500
              )
                |> Font.color
            ]
            (text "h")
        ]


minuteInput :
    { model
        | backdrop : Backdrop
        , images : Images
        , theme : Theme
    }
    -> Modal
    -> Element Msg
minuteInput { theme } (Modal { minutes }) =
    row
        [ width fill
        , height fill
        , paddingEach
            { top = 0
            , right = 8
            , bottom = 0
            , left = 0
            }
        , Border.width 1
        , Border.solid
        , (if minutes |> isMinuteCorrect then
            theme |> ThemeColor.textboxBorder

           else
            Color.negative500
          )
            |> Border.color
        , Border.rounded 8
        ]
        [ Input.text
            [ width fill
            , height shrink
            , centerY
            , Background.color Color.none
            , Border.color Color.none
            , Font.bold
            , Font.size 14
            , (if minutes |> isMinuteCorrect then
                theme |> ThemeColor.text

               else
                Color.negative500
              )
                |> Font.color
            ]
            { onChange = InputMinute
            , text = minutes
            , placeholder =
                Input.placeholder
                    [ (if minutes |> isMinuteCorrect then
                        theme |> ThemeColor.placeholder2

                       else
                        Color.negative500
                      )
                        |> Font.color
                    ]
                    (text "00")
                    |> Just
            , label = Input.labelHidden "Maturity Time (Minutes)"
            }
        , el
            [ width shrink
            , height shrink
            , centerY
            , Font.bold
            , Font.size 14
            , (if minutes |> isMinuteCorrect then
                theme |> ThemeColor.text

               else
                Color.negative500
              )
                |> Font.color
            ]
            (text "m")
        ]


posixInput :
    { model
        | backdrop : Backdrop
        , images : Images
        , theme : Theme
        , time : Posix
    }
    -> Modal
    -> Element Msg
posixInput { theme, time } (Modal { posixTime }) =
    row
        [ width fill
        , height fill
        , paddingEach
            { top = 0
            , right = 8
            , bottom = 0
            , left = 0
            }
        , Border.width 1
        , Border.solid
        , (if posixTime |> isPosixCorrect time then
            theme |> ThemeColor.textboxBorder

           else
            Color.negative500
          )
            |> Border.color
        , Border.rounded 8
        ]
        [ Input.text
            [ width fill
            , height shrink
            , centerY
            , Background.color Color.none
            , Border.color Color.none
            , Font.bold
            , Font.size 14
            , (if posixTime |> isPosixCorrect time then
                theme |> ThemeColor.text

               else
                Color.negative500
              )
                |> Font.color
            ]
            { onChange = InputPosix
            , text = posixTime
            , placeholder =
                Input.placeholder
                    [ (if posixTime |> isPosixCorrect time then
                        theme |> ThemeColor.placeholder2

                       else
                        Color.negative500
                      )
                        |> Font.color
                    ]
                    (text "0")
                    |> Just
            , label = Input.labelHidden "Maturity Time (Unix)"
            }
        ]


doneButton : Theme -> Modal -> Element Msg
doneButton theme (Modal { posixTime }) =
    Input.button
        [ Region.description "Done button"
        , width fill
        , height <| px 44
        , theme |> ThemeColor.primaryBtn |> Background.color
        , Border.rounded 4
        ]
        { onPress = Just (SelectedMaturity (posixTime |> stringToPosix |> posixToMaturity))
        , label =
            el
                [ centerX
                , centerY
                , Font.size 16
                , paddingXY 0 4
                , Font.color Color.light100
                ]
                (text "Done")
        }


calendarSettings : Theme -> DatePicker.Settings
calendarSettings theme =
    { firstDayOfWeek = Mon
    , language = Nothing
    , disabled = always False
    , pickerAttributes =
        [ Border.width 1
        , Border.color (Element.rgb255 186 189 182)
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = 3
            , bottomRight = 3
            }
        , Element.moveDown 3
        , padding 8
        , spacing 4
        , Element.centerX
        , Element.centerY
        , Element.width Element.fill
        , theme |> ThemeColor.background |> Background.color
        ]
    , headerAttributes =
        [ Element.width Element.fill
        , padding 2
        , Font.bold
        ]
    , tableAttributes =
        [ Element.height Element.fill
        , Element.centerY
        ]
    , weekdayAttributes = [ Font.bold ]
    , dayAttributes =
        [ Border.rounded 3
        , padding 3
        , Element.width Element.fill
        , Font.center
        , Element.centerY
        , Element.mouseOver [ Background.color (Element.rgb255 0x73 0xB6 0xFF) ]
        ]
    , monthYearAttribute =
        [ Border.rounded 3
        , padding 11
        , Element.width Element.fill
        , Font.center
        , Element.centerY
        , Element.mouseOver [ Background.color (Element.rgb255 0x73 0xB6 0xFF) ]
        ]
    , wrongMonthDayAttributes =
        [ Font.color (Element.rgb255 0x80 0x80 0x80) ]
    , todayDayAttributes =
        [ Background.color (Element.rgb255 0xFF 0xC1 0x9B) ]
    , selectedDayAttributes =
        [ Background.color (Element.rgb255 0x00 0x7B 0xFF) ]
    , disabledDayAttributes =
        [ Font.color (Element.rgb255 0x80 0x80 0x80)
        , Background.color (Element.rgb255 0xDD 0xDD 0xDD)
        ]
    , monthsTableAttributes =
        [ Element.spaceEvenly
        , Element.width Element.fill
        , Element.height Element.fill
        ]
    , yearsTableAttributes =
        [ Element.spaceEvenly
        , Element.width Element.fill
        , Element.height Element.fill
        ]
    , headerButtonsAttributes =
        [ Element.pointer
        , padding 6
        , Border.rounded 3
        , Border.shadow
            { offset = ( 1, 1 )
            , size = 1
            , blur = 1
            , color = Element.rgb255 186 189 182
            }
        , Element.mouseOver
            [ Border.shadow
                { offset = ( 1, 1 )
                , size = 2
                , blur = 1
                , color = Element.rgb255 186 189 182
                }
            ]
        ]
    , previousMonthElement =
        Element.text "🢐"
    , nextMonthElement =
        Element.text "🢒"
    }


isHourCorrect : String -> Bool
isHourCorrect string =
    string
        |> String.toInt
        |> Maybe.map
            (\hour ->
                if hour >= 0 && hour < 24 then
                    True

                else
                    False
            )
        |> Maybe.withDefault False


isMinuteCorrect : String -> Bool
isMinuteCorrect string =
    string
        |> String.toInt
        |> Maybe.map
            (\minutes ->
                if minutes >= 0 && minutes <= 59 then
                    True

                else
                    False
            )
        |> Maybe.withDefault False


isPosixCorrect : Posix -> String -> Bool
isPosixCorrect now maturityStr =
    let
        maturityPosix =
            maturityStr
                |> String.toInt
                |> Maybe.map
                    (\int -> int * 1000)
                |> Maybe.withDefault 0
    in
    (now |> posixToMillis) < maturityPosix


stringToPosix : String -> Posix
stringToPosix string =
    string
        |> String.toInt
        |> Maybe.map
            (\int -> int * 1000)
        |> Maybe.withDefault 0
        |> millisToPosix
