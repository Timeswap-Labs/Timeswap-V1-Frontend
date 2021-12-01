module Services.Settings.Main exposing
    ( Msg
    , Service
    , Settings
    , getDeadline
    , getSlippage
    , hasDeadlineInput
    , hasSlippageInput
    , init
    , initSettings
    , inputDeadline
    , inputSlippage
    , update
    , view
    )

import Data.Backdrop exposing (Backdrop)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Slippage as Slippage exposing (Slippage)
import Element
    exposing
        ( Element
        , alignBottom
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , inFront
        , none
        , onRight
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
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (OptionState)
import Html.Attributes
import Services.Settings.Tooltip as Tooltip exposing (Tooltip)
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Input as Input


type alias Settings =
    { slippage : Maybe String
    , deadline : Maybe String
    }


type Service
    = Service (Maybe Tooltip)


initSettings : Settings
initSettings =
    { slippage = Nothing
    , deadline = Nothing
    }


init : Service
init =
    Service Nothing


type Msg
    = OnMouseEnter Tooltip
    | OnMouseLeave


update : Msg -> Service
update msg =
    case msg of
        OnMouseEnter tooltip ->
            Just tooltip
                |> Service

        OnMouseLeave ->
            Service Nothing


inputSlippage : String -> Settings -> Settings
inputSlippage string settings =
    if string |> Input.isFloat then
        { settings | slippage = Just string }

    else
        settings


inputDeadline : String -> Settings -> Settings
inputDeadline string settings =
    if string |> Input.isInt then
        { settings | deadline = Just string }

    else
        settings


getSlippage : Settings -> Maybe Slippage
getSlippage { slippage } =
    slippage |> Maybe.map Slippage.fromString


getDeadline : Settings -> Maybe Deadline
getDeadline { deadline } =
    deadline |> Maybe.map Deadline.fromString


hasSlippageInput : Settings -> Bool
hasSlippageInput { slippage } =
    slippage
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


hasDeadlineInput : Settings -> Bool
hasDeadlineInput { deadline } =
    deadline
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


view :
    { msgs
        | exitSettings : msg
        , chooseSlippageOption : Slippage.Option -> msg
        , chooseDeadlineOption : Deadline.Option -> msg
        , inputSlippage : String -> msg
        , inputDeadline : String -> msg
    }
    ->
        { model
            | device : Device
            , backdrop : Backdrop
            , slippage : Slippage
            , deadline : Deadline
            , images : Images
        }
    -> Service
    -> Settings
    -> Element (Or Msg msg)
view msgs ({ device, backdrop, images } as model) (Service tooltip) settings =
    column
        ([ padding 40
         , spacing 32
         , centerX
         , centerY
         , inFront (Exit.buttonWithMsg images msgs.exitSettings |> Element.map Or)
         ]
            ++ Glass.lightPrimaryModal backdrop 0
            ++ (if Device.isPhone device then
                    [ width fill
                    , height shrink
                    , alignBottom
                    ]

                else
                    [ width <| px 533
                    , height shrink
                    ]
               )
        )
        [ title
        , content msgs model tooltip settings
        ]


title : Element msg
title =
    el
        [ width shrink
        , height shrink
        , paddingXY 0 4
        , centerX
        , Font.family [ Font.typeface "Supreme" ]
        , Font.bold
        , Font.size 24
        , Font.color Color.light100
        ]
        (text "Transaction Settings")


content :
    { msgs
        | exitSettings : msg
        , chooseSlippageOption : Slippage.Option -> msg
        , chooseDeadlineOption : Deadline.Option -> msg
        , inputSlippage : String -> msg
        , inputDeadline : String -> msg
    }
    ->
        { model
            | device : Device
            , backdrop : Backdrop
            , slippage : Slippage
            , deadline : Deadline
            , images : Images
        }
    -> Maybe Tooltip
    -> Settings
    -> Element (Or Msg msg)
content msgs ({ images } as model) tooltip settings =
    column
        [ width fill
        , height shrink
        , spacing 20
        ]
        [ column
            [ width fill
            , height shrink
            , spacing 12
            ]
            [ row
                [ width shrink
                , height shrink
                , spacing 8
                ]
                [ el
                    [ width shrink
                    , height shrink
                    , paddingXY 0 4
                    , Font.regular
                    , Font.size 16
                    , Font.color Color.transparent300
                    ]
                    (text "Slippage tolerance")
                , Image.info images
                    [ width <| px 16
                    , centerY
                    , Events.onMouseEnter (OnMouseEnter Tooltip.Slippage |> Either)
                    , Events.onMouseLeave (OnMouseLeave |> Either)
                    , (case tooltip of
                        Just Tooltip.Slippage ->
                            Tooltip.slippage

                        _ ->
                            none
                      )
                        |> onRight
                    ]
                ]
            , row
                [ width fill
                , height shrink
                , spacing 8
                ]
                [ slippageSwitch msgs model |> Element.map Or
                , slippageInput msgs model settings |> Element.map Or
                ]
            ]
        , column
            [ width fill
            , height shrink
            , spacing 12
            ]
            [ row
                [ width shrink
                , height shrink
                , spacing 8
                ]
                [ el
                    [ width shrink
                    , height shrink
                    , paddingXY 0 4
                    , Font.regular
                    , Font.size 16
                    , Font.color Color.transparent300
                    ]
                    (text "Transaction deadline")
                , Image.info images
                    [ width <| px 16
                    , centerY
                    , Events.onMouseEnter (OnMouseEnter Tooltip.Deadline |> Either)
                    , Events.onMouseLeave (OnMouseLeave |> Either)
                    , (case tooltip of
                        Just Tooltip.Deadline ->
                            Tooltip.deadline

                        _ ->
                            none
                      )
                        |> onRight
                    ]
                ]
            , row
                [ width fill
                , height shrink
                , spacing 8
                ]
                [ deadlineSwitch msgs model |> Element.map Or
                , deadlineInput msgs model settings |> Element.map Or
                ]
            ]
        ]


slippageSwitch :
    { msgs | chooseSlippageOption : Slippage.Option -> msg }
    -> { model | device : Device, slippage : Slippage }
    -> Element msg
slippageSwitch msgs ({ device, slippage } as model) =
    Input.radioRow
        ([ height <| px 54
         , padding 4
         , spacing 4
         , Background.color Color.transparent100
         , Border.rounded 8
         , htmlAttribute <| Html.Attributes.id "slippage"
         ]
            ++ (if Device.isPhone device then
                    [ width <| px 199 ]

                else
                    [ width <| px 304 ]
               )
        )
        { onChange = msgs.chooseSlippageOption
        , options =
            [ ( Slippage.Small, "0.10%" )
            , ( Slippage.Medium, "0.50%" )
            , ( Slippage.Large, "1.00%" )
            ]
                |> List.map
                    (\( options, label ) ->
                        radio model label
                            |> Input.optionWith options
                    )
        , selected = slippage |> Slippage.toOption
        , label = Input.labelHidden "Slippage Switch"
        }


deadlineSwitch :
    { msgs | chooseDeadlineOption : Deadline.Option -> msg }
    -> { model | device : Device, deadline : Deadline }
    -> Element msg
deadlineSwitch msgs ({ device, deadline } as model) =
    Input.radioRow
        [ (if Device.isPhone device then
            px 199

           else
            px 304
          )
            |> width
        , height <| px 54
        , padding 4
        , spacing 4
        , Background.color Color.transparent100
        , Border.rounded 8
        , htmlAttribute <| Html.Attributes.id "deadline"
        ]
        { onChange = msgs.chooseDeadlineOption
        , options =
            [ ( Deadline.Short, "10m" )
            , ( Deadline.Medium, "20m" )
            , ( Deadline.Long, "30m" )
            ]
                |> List.map
                    (\( options, label ) ->
                        radio model label
                            |> Input.optionWith options
                    )
        , selected = deadline |> Deadline.toOption
        , label = Input.labelHidden "Deadline Switch"
        }


radio : { model | device : Device } -> String -> OptionState -> Element msg
radio { device } label optionState =
    el
        ([ (if Device.isPhone device then
                px 61

            else
                px 96
           )
            |> width
         , height fill
         , Border.rounded 4
         ]
            ++ (case optionState of
                    Input.Selected ->
                        [ Background.color Color.primary500 ]

                    _ ->
                        []
               )
        )
        (el
            [ centerX
            , centerY
            , Font.bold
            , Font.size 16
            , (case optionState of
                Input.Selected ->
                    Color.transparent500

                _ ->
                    Color.transparent300
              )
                |> Font.color
            ]
            (text label)
        )


slippageInput :
    { msgs | inputSlippage : String -> msg }
    -> { model | slippage : Slippage }
    -> { settings | slippage : Maybe String }
    -> Element msg
slippageInput msgs { slippage } settings =
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
        , (settings.slippage
            |> Maybe.map
                (\string ->
                    if string |> Slippage.isCorrect then
                        Color.transparent100

                    else
                        Color.negative500
                )
            |> Maybe.withDefault Color.transparent100
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
            , Font.size 16
            , (settings.slippage
                |> Maybe.map
                    (\string ->
                        if string |> Slippage.isCorrect then
                            Color.transparent500

                        else
                            Color.negative500
                    )
                |> Maybe.withDefault Color.transparent500
              )
                |> Font.color
            , htmlAttribute <| Html.Attributes.id "slippage"
            ]
            { onChange = msgs.inputSlippage
            , text =
                settings.slippage
                    |> Maybe.withDefault
                        (slippage
                            |> Slippage.toString
                            |> Maybe.withDefault ""
                        )
            , placeholder =
                Input.placeholder
                    [ (settings.slippage
                        |> Maybe.map
                            (\string ->
                                if string |> Slippage.isCorrect then
                                    Color.transparent100

                                else
                                    Color.negative500
                            )
                        |> Maybe.withDefault Color.transparent100
                      )
                        |> Font.color
                    ]
                    (text "50.00")
                    |> Just
            , label = Input.labelHidden "Slippage Input"
            }
        , el
            [ width shrink
            , height shrink
            , centerY
            , Font.bold
            , Font.size 16
            , (settings.slippage
                |> Maybe.map
                    (\string ->
                        if string |> Slippage.isCorrect then
                            Color.transparent500

                        else
                            Color.negative500
                    )
                |> Maybe.withDefault Color.transparent500
              )
                |> Font.color
            ]
            (text "%")
        ]


deadlineInput :
    { msgs | inputDeadline : String -> msg }
    -> { model | deadline : Deadline }
    -> { settings | deadline : Maybe String }
    -> Element msg
deadlineInput msgs { deadline } settings =
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
        , (settings.deadline
            |> Maybe.map
                (\string ->
                    if string |> Deadline.isCorrect then
                        Color.transparent100

                    else
                        Color.negative500
                )
            |> Maybe.withDefault Color.transparent100
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
            , Font.size 16
            , (settings.deadline
                |> Maybe.map
                    (\string ->
                        if string |> Deadline.isCorrect then
                            Color.transparent500

                        else
                            Color.negative500
                    )
                |> Maybe.withDefault Color.transparent500
              )
                |> Font.color
            , htmlAttribute <| Html.Attributes.id "deadline"
            ]
            { onChange = msgs.inputDeadline
            , text =
                settings.deadline
                    |> Maybe.withDefault
                        (deadline
                            |> Deadline.toString
                            |> Maybe.withDefault ""
                        )
            , placeholder =
                Input.placeholder
                    [ (settings.deadline
                        |> Maybe.map
                            (\string ->
                                if string |> Deadline.isCorrect then
                                    Color.transparent100

                                else
                                    Color.negative500
                            )
                        |> Maybe.withDefault Color.transparent100
                      )
                        |> Font.color
                    ]
                    (text "180")
                    |> Just
            , label = Input.labelHidden "Deadline Input"
            }
        , el
            [ width shrink
            , height shrink
            , centerY
            , Font.bold
            , Font.size 16
            , (settings.deadline
                |> Maybe.map
                    (\string ->
                        if string |> Deadline.isCorrect then
                            Color.transparent500

                        else
                            Color.negative500
                    )
                |> Maybe.withDefault Color.transparent500
              )
                |> Font.color
            ]
            (text "m")
        ]