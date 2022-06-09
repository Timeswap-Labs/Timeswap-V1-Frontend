module Modal.MaturityPicker.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Data.Backdrop exposing (Backdrop)
import Data.Images exposing (Images)
import Data.Maturity exposing (Maturity, posixToMaturity, toString)
import Data.Offset as Offset exposing (Offset)
import Data.Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Theme as Theme exposing (Theme)
import Element
    exposing
        ( Element
        , alignRight
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
import SingleDatePicker
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.Input as Input
import Utility.Maybe as Maybe
import Utility.ThemeColor as ThemeColor


type Modal
    = Modal
        { dateText : String
        , picker : SingleDatePicker.DatePicker
        , pickedTime : Maybe Posix
        , hours : String
        , minutes : String
        , pair : Pair
        }


type Msg
    = OpenPicker
      -- | ClosePicker
    | UpdatePicker ( SingleDatePicker.DatePicker, Maybe Posix )
    | InputHour String
    | InputMinute String
    | SelectedMaturity Maturity
    | Exit


type Effect
    = InputMaturity Pool


init : Pair -> Posix -> ( Modal, Cmd Msg )
init pair time =
    ( { dateText = ""
      , picker = SingleDatePicker.init
      , pickedTime = time |> Just
      , hours = "00"
      , minutes = "00"
      , pair = pair
      }
        |> Modal
    , Cmd.none
    )


subscriptions : Offset -> Modal -> Sub Msg
subscriptions offset (Modal modal) =
    SingleDatePicker.subscriptions (datePickerSettings offset) UpdatePicker modal.picker


datePickerSettings : Offset -> SingleDatePicker.Settings Msg
datePickerSettings offset =
    SingleDatePicker.defaultSettings (offset |> Offset.toZone) UpdatePicker


update :
    { model
        | offset : Offset
        , time : Posix
    }
    -> Msg
    -> Modal
    -> ( Maybe Modal, Maybe Effect )
update { offset, time } msg (Modal modal) =
    case msg of
        OpenPicker ->
            ( { modal
                | picker = SingleDatePicker.openPicker (datePickerSettings offset) time modal.pickedTime modal.picker
              }
                |> Modal
                |> Just
            , Nothing
            )

        -- ClosePicker ->
        --     ( { modal
        --         | picker = SingleDatePicker.closePicker modal.picker
        --       }
        --         |> Modal
        --         |> Just
        --     , Nothing
        --     )
        UpdatePicker ( newPicker, maybeNewTime ) ->
            ( { modal
                | picker = newPicker
                , pickedTime =
                    Maybe.map (\t -> Just t) maybeNewTime
                        |> Maybe.withDefault modal.pickedTime
                , dateText =
                    case maybeNewTime of
                        Just newTimePosix ->
                            newTimePosix |> posixToMaturity |> toString (offset |> Offset.toZone)

                        _ ->
                            modal.dateText
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
        , offset : Offset
    }
    -> Modal
    -> Element Msg
view ({ backdrop, theme, time } as model) ((Modal { pickedTime }) as modal) =
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
                            , Font.size 16
                            , Font.bold
                            , paddingXY 0 3
                            , theme |> ThemeColor.text |> Font.color
                            ]
                            (text "Choose Maturity")
                        , IconButton.exit model Exit
                        ]
                    , dateField model modal

                    -- , row
                    --     [ width fill
                    --     , height shrink
                    --     , spacing 12
                    --     ]
                    --     [ hourInput model modal
                    --     , minuteInput model modal
                    --     ]
                    , if pickedTime |> isPosixValid time then
                        doneButton theme modal

                      else
                        Button.disabled theme "Done" |> map never
                    ]
                )
        }


dateField :
    { model
        | backdrop : Backdrop
        , images : Images
        , theme : Theme
        , time : Posix
        , offset : Offset
    }
    -> Modal
    -> Element Msg
dateField { images, theme, offset } (Modal { picker, dateText }) =
    column
        [ width fill, spacing 10 ]
        [ el
            [ Font.size 14
            , theme |> ThemeColor.textLight |> Font.color
            ]
            (text "Date")
        , Input.button
            [ Region.description "Date Picker button"
            , width fill
            , paddingXY 12 5
            , height <| px 44
            , theme |> ThemeColor.btnBackground |> Background.color
            , Border.width 1
            , theme |> ThemeColor.border |> Border.color
            , Border.rounded 8
            ]
            { onPress =
                -- if picker |> SingleDatePicker.isOpen then
                --     Just ClosePicker
                -- else
                Just OpenPicker
            , label =
                row
                    [ width fill
                    , centerY
                    , Font.size 14
                    , paddingXY 0 4
                    , theme |> ThemeColor.text |> Font.color
                    ]
                    [ if dateText == "" then
                        text "Choose Date"

                      else
                        text dateText
                    , images
                        |> (case theme of
                                Theme.Dark ->
                                    Image.discloser

                                Theme.Light ->
                                    Image.arrowDownDark
                           )
                            [ width <| px 11
                            , height <| px 7
                            , alignRight
                            , centerY
                            ]
                    ]
            }
        , column [ width fill ]
            [ SingleDatePicker.view (datePickerSettings offset) picker |> Element.html ]
        ]


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


doneButton : Theme -> Modal -> Element Msg
doneButton theme (Modal { pickedTime }) =
    Input.button
        [ Region.description "Done button"
        , width fill
        , height <| px 44
        , theme |> ThemeColor.primaryBtn |> Background.color
        , Border.rounded 4
        ]
        { onPress =
            case pickedTime of
                Just timeInPosix ->
                    Just (SelectedMaturity (timeInPosix |> posixToMaturity))

                _ ->
                    Nothing
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


isPosixValid : Posix -> Maybe Posix -> Bool
isPosixValid now pickedTime =
    case pickedTime of
        Just posixTime ->
            (now |> posixToMillis) < (posixTime |> posixToMillis)

        _ ->
            False
