module Services.Settings.Main exposing
    ( Msgs
    , Settings
    , getDeadline
    , getSlippage
    , hasDeadlineInput
    , hasSlippageInput
    , init
    , inputDeadline
    , inputSlippage
    , toUrl
    , view
    )

import Data.Backdrop exposing (Backdrop)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Device as Device exposing (Device)
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
import Element.Input as Input exposing (OptionState)
import Html.Attributes
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Input as Input


type alias Settings =
    { slippage : Maybe String
    , deadline : Maybe String
    }


init : Settings
init =
    { slippage = Nothing
    , deadline = Nothing
    }


toUrl : String
toUrl =
    "#settings"


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


type alias Msgs msg =
    { chooseSlippageOption : Slippage.Option -> msg
    , chooseDeadlineOption : Deadline.Option -> msg
    , inputSlippage : String -> msg
    , inputDeadline : String -> msg
    }


view :
    Msgs msg
    ->
        { model
            | device : Device
            , backdrop : Backdrop
            , slippage : Slippage
            , deadline : Deadline
        }
    -> Settings
    -> Element msg
view msgs ({ device, backdrop } as model) settings =
    column
        ([ padding 40
         , spacing 32
         , centerX
         , centerY
         , inFront Exit.button
         ]
            ++ Glass.darkPrimaryModal backdrop 0
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
        , content msgs model settings
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
    Msgs msg
    ->
        { model
            | device : Device
            , backdrop : Backdrop
            , slippage : Slippage
            , deadline : Deadline
        }
    -> Settings
    -> Element msg
content msgs model settings =
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
            [ el
                [ width shrink
                , height shrink
                , paddingXY 0 4
                , Font.regular
                , Font.size 16
                , Font.color Color.transparent300
                ]
                (text "Slippage tolerance")
            , row
                [ width fill
                , height shrink
                , spacing 8
                ]
                [ slippageSwitch msgs model
                , slippageInput msgs model settings
                ]
            ]
        , column
            [ width fill
            , height shrink
            , spacing 12
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
            , row
                [ width fill
                , height shrink
                , spacing 8
                ]
                [ deadlineSwitch msgs model
                , deadlineInput msgs model settings
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
         ]
            ++ (if Device.isPhone device then
                    [ width <| px 199 ]

                else
                    [ width <| px 304 ]
               )
        )
        { onChange = msgs.chooseSlippageOption
        , options =
            [ ( Slippage.Small, "0.1%" )
            , ( Slippage.Medium, "0.5%" )
            , ( Slippage.Large, "1%" )
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
