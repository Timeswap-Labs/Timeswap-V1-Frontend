port module Modal.Settings.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Data.Backdrop exposing (Backdrop)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.PriceFeed as PriceFeed exposing (PriceFeed)
import Data.Slippage as Slippage exposing (Slippage)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , htmlAttribute
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
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Modal.Settings.Tooltip exposing (Tooltip)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Input as Input


type Modal
    = Modal
        { slippage : Or Slippage.Option String
        , deadline : Or Deadline.Option String
        , priceFeed : PriceFeed
        , tooltip : Maybe Tooltip
        }


type Msg
    = ChooseSlippageOption Slippage.Option
    | ChooseDeadlineOption Deadline.Option
    | ChoosePriceFeed PriceFeed
    | InputSlippage String
    | InputDeadline String
    | ClickOutsideSlippage
    | ClickOutsideDeadline
    | OnMouseEnter Tooltip
    | OnMouseLeave
    | Exit


type Effect
    = UpdateSlippage Slippage
    | UpdateDeadline Deadline
    | UpdatePriceFeed PriceFeed


init :
    { model
        | slippage : Slippage
        , deadline : Deadline
        , priceFeed : PriceFeed
    }
    -> Modal
init { slippage, deadline, priceFeed } =
    { slippage = slippage |> Slippage.toSettings
    , deadline = deadline |> Deadline.toSettings
    , priceFeed = priceFeed
    , tooltip = Nothing
    }
        |> Modal


update : Msg -> Modal -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update msg (Modal modal) =
    case msg of
        ChooseSlippageOption option ->
            option
                |> Left
                |> (\slippage ->
                        ( { modal | slippage = slippage }
                            |> Modal
                            |> Just
                        , slippage
                            |> Slippage.fromSettings
                            |> Slippage.encode
                            |> cacheSlippage
                        , slippage
                            |> Slippage.fromSettings
                            |> UpdateSlippage
                            |> Just
                        )
                   )

        ChooseDeadlineOption option ->
            option
                |> Left
                |> (\deadline ->
                        ( { modal | deadline = deadline }
                            |> Modal
                            |> Just
                        , deadline
                            |> Deadline.fromSettings
                            |> Deadline.encode
                            |> cacheDeadline
                        , deadline
                            |> Deadline.fromSettings
                            |> UpdateDeadline
                            |> Just
                        )
                   )

        ChoosePriceFeed priceFeed ->
            ( { modal | priceFeed = priceFeed }
                |> Modal
                |> Just
            , priceFeed
                |> PriceFeed.encode
                |> cachePriceFeed
            , priceFeed
                |> UpdatePriceFeed
                |> Just
            )

        InputSlippage input ->
            if input |> Input.isFloat then
                input
                    |> Right
                    |> (\slippage ->
                            ( { modal | slippage = slippage }
                                |> Modal
                                |> Just
                            , slippage
                                |> Slippage.fromSettings
                                |> Slippage.encode
                                |> cacheSlippage
                            , slippage
                                |> Slippage.fromSettings
                                |> UpdateSlippage
                                |> Just
                            )
                       )

            else
                ( modal
                    |> Modal
                    |> Just
                , Cmd.none
                , Nothing
                )

        InputDeadline input ->
            if input |> Input.isInt then
                input
                    |> Right
                    |> (\deadline ->
                            ( { modal | deadline = deadline }
                                |> Modal
                                |> Just
                            , deadline
                                |> Deadline.fromSettings
                                |> Deadline.encode
                                |> cacheSlippage
                            , deadline
                                |> Deadline.fromSettings
                                |> UpdateDeadline
                                |> Just
                            )
                       )

            else
                ( modal
                    |> Modal
                    |> Just
                , Cmd.none
                , Nothing
                )

        ClickOutsideSlippage ->
            ( { modal
                | slippage =
                    modal.slippage
                        |> Slippage.fromSettings
                        |> Slippage.toSettings
              }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ClickOutsideDeadline ->
            ( { modal
                | deadline =
                    modal.deadline
                        |> Deadline.fromSettings
                        |> Deadline.toSettings
              }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        OnMouseEnter tooltip ->
            ( { modal | tooltip = Just tooltip }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        OnMouseLeave ->
            ( { modal | tooltip = Nothing }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        Exit ->
            ( Nothing
            , Cmd.none
            , Nothing
            )


port cacheSlippage : Value -> Cmd msg


port cacheDeadline : Value -> Cmd msg


port cachePriceFeed : Value -> Cmd msg


subscriptions : Modal -> Sub Msg
subscriptions (Modal modal) =
    [ onClickOutsideSlippage modal
    , onClickOutsideDeadline modal
    ]
        |> Sub.batch


onClickOutsideSlippage :
    { modal | slippage : Or Slippage.Option String }
    -> Sub Msg
onClickOutsideSlippage { slippage } =
    slippage
        |> Slippage.toString
        |> Maybe.map
            (\_ ->
                decoderOutsideSlippage
                    |> Decode.at [ "target", "id" ]
                    |> Browser.Events.onClick
            )
        |> Maybe.withDefault Sub.none


decoderOutsideSlippage : Decoder Msg
decoderOutsideSlippage =
    Decode.string
        |> Decode.andThen
            (\string ->
                if string /= "slippage" then
                    Decode.succeed ClickOutsideSlippage

                else
                    Decode.fail "Its the slippage input"
            )


onClickOutsideDeadline :
    { modal | deadline : Or Deadline.Option String }
    -> Sub Msg
onClickOutsideDeadline { deadline } =
    deadline
        |> Deadline.toString
        |> Maybe.map
            (\_ ->
                decoderOutsideDeadline
                    |> Decode.at [ "target", "id" ]
                    |> Browser.Events.onClick
            )
        |> Maybe.withDefault Sub.none


decoderOutsideDeadline : Decoder Msg
decoderOutsideDeadline =
    Decode.string
        |> Decode.andThen
            (\string ->
                if string /= "deadline" then
                    Decode.succeed ClickOutsideDeadline

                else
                    Decode.fail "Its the deadline input"
            )


view :
    { model
        | backdrop : Backdrop
        , images : Images
    }
    -> Modal
    -> Element Msg
view model (Modal modal) =
    Glass.outsideModal model
        { onClick = Exit
        , modal =
            column
                [ width <| px 375
                , height shrink
                , padding 24
                , spacing 16
                , centerX
                , centerY
                , Background.color Color.background
                , Border.rounded 8
                , Border.color Color.transparent100
                , Border.width 1
                ]
                [ column
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
                            (text "Settings")
                        , IconButton.exit model Exit
                        ]
                    ]
                , slippageSetting modal
                , deadlineSetting modal
                ]
        }


slippageSetting :
    { modal | slippage : Or Slippage.Option String }
    -> Element Msg
slippageSetting modal =
    column
        [ width fill
        , height shrink
        , spacing 8
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
            ]
        , row
            [ width fill
            , height shrink
            , spacing 8
            ]
            [ slippageSwitch modal
            , slippageInput modal
            ]
        ]


deadlineSetting :
    { modal | deadline : Or Deadline.Option String }
    -> Element Msg
deadlineSetting modal =
    column
        [ width fill
        , height shrink
        , spacing 8
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
            ]
        , row
            [ width fill
            , height shrink
            , spacing 8
            ]
            [ deadlineSwitch modal
            , deadlineInput modal
            ]
        ]


slippageSwitch :
    { modal | slippage : Or Slippage.Option String }
    -> Element Msg
slippageSwitch { slippage } =
    Input.radioRow
        [ width shrink
        , height shrink
        , padding 4
        , spacing 4
        , Background.color Color.transparent100
        , Border.rounded 8
        ]
        { onChange = ChooseSlippageOption
        , options =
            [ ( Slippage.Small, "0.10%" )
            , ( Slippage.Medium, "0.50%" )
            , ( Slippage.Large, "1.00%" )
            ]
                |> List.map
                    (\( options, label ) ->
                        radio label
                            |> Input.optionWith options
                    )
        , selected = slippage |> Slippage.toOption
        , label = Input.labelHidden "Slippage Switch"
        }


deadlineSwitch :
    { modal | deadline : Or Deadline.Option String }
    -> Element Msg
deadlineSwitch { deadline } =
    Input.radioRow
        [ width shrink
        , height shrink
        , padding 4
        , spacing 4
        , Background.color Color.transparent100
        , Border.rounded 8
        ]
        { onChange = ChooseDeadlineOption
        , options =
            [ ( Deadline.Short, "10m" )
            , ( Deadline.Medium, "20m" )
            , ( Deadline.Long, "30m" )
            ]
                |> List.map
                    (\( options, label ) ->
                        radio label
                            |> Input.optionWith options
                    )
        , selected = deadline |> Deadline.toOption
        , label = Input.labelHidden "Deadline Switch"
        }


slippageInput :
    { modal | slippage : Or Slippage.Option String }
    -> Element Msg
slippageInput { slippage } =
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
        , (slippage
            |> Slippage.toString
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
        , htmlAttribute <| Html.Attributes.id "slippage"
        ]
        [ Input.text
            [ width fill
            , height shrink
            , centerY
            , Background.color Color.none
            , Border.color Color.none
            , Font.bold
            , Font.size 16
            , (slippage
                |> Slippage.toString
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
            { onChange = InputSlippage
            , text =
                slippage
                    |> Slippage.toString
                    |> Maybe.withDefault ""
            , placeholder =
                Input.placeholder
                    [ (slippage
                        |> Slippage.toString
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
            , (slippage
                |> Slippage.toString
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
    { modal | deadline : Or Deadline.Option String }
    -> Element Msg
deadlineInput { deadline } =
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
        , (deadline
            |> Deadline.toString
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
        , htmlAttribute <| Html.Attributes.id "deadline"
        ]
        [ Input.text
            [ width fill
            , height shrink
            , centerY
            , Background.color Color.none
            , Border.color Color.none
            , Font.bold
            , Font.size 16
            , (deadline
                |> Deadline.toString
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
            { onChange = InputDeadline
            , text =
                deadline
                    |> Deadline.toString
                    |> Maybe.withDefault ""
            , placeholder =
                Input.placeholder
                    [ (deadline
                        |> Deadline.toString
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
            , (deadline
                |> Deadline.toString
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


radio : String -> OptionState -> Element msg
radio label optionState =
    el
        ([ width <| px 59
         , height <| px 36
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
