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
import Data.Theme exposing (Theme)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
        , paddingEach
        , paddingXY
        , pointer
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (OptionState)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Modal.Outside as Outside
import Modal.Settings.Tooltip exposing (Tooltip)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Id as Id
import Utility.Input as Input
import Utility.ThemeColor as ThemeColor


type Modal
    = Modal
        { slippage : Or Slippage String
        , deadline : Or Deadline String
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
    { slippage = slippage |> Left
    , deadline = deadline |> Left
    , priceFeed = priceFeed
    , tooltip = Nothing
    }
        |> Modal


update : Msg -> Modal -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update msg (Modal modal) =
    case msg of
        ChooseSlippageOption option ->
            option
                |> Slippage.fromOption
                |> (\slippage ->
                        ( { modal | slippage = Left slippage }
                            |> Modal
                            |> Just
                        , slippage
                            |> Slippage.encode
                            |> cacheSlippage
                        , slippage
                            |> UpdateSlippage
                            |> Just
                        )
                   )

        ChooseDeadlineOption option ->
            option
                |> Deadline.fromOption
                |> (\deadline ->
                        ( { modal | deadline = Left deadline }
                            |> Modal
                            |> Just
                        , deadline
                            |> Deadline.encode
                            |> cacheDeadline
                        , deadline
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
                    |> Slippage.fromString
                    |> (\slippage ->
                            ( { modal | slippage = Right input }
                                |> Modal
                                |> Just
                            , slippage
                                |> Slippage.encode
                                |> cacheSlippage
                            , slippage
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
                    |> Deadline.fromString
                    |> (\deadline ->
                            ( { modal | deadline = Right input }
                                |> Modal
                                |> Just
                            , deadline
                                |> Deadline.encode
                                |> cacheDeadline
                            , deadline
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
            ( (case modal.slippage of
                Left _ ->
                    modal

                Right input ->
                    { modal
                        | slippage =
                            input
                                |> Slippage.fromString
                                |> Left
                    }
              )
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ClickOutsideDeadline ->
            ( (case modal.deadline of
                Left _ ->
                    modal

                Right input ->
                    { modal
                        | deadline =
                            input
                                |> Deadline.fromString
                                |> Left
                    }
              )
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
    { modal | slippage : Or Slippage String }
    -> Sub Msg
onClickOutsideSlippage { slippage } =
    case slippage of
        Left _ ->
            Sub.none

        Right _ ->
            decoderOutsideSlippage
                |> Decode.at [ "target", "id" ]
                |> Browser.Events.onClick


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
    { modal | deadline : Or Deadline String }
    -> Sub Msg
onClickOutsideDeadline { deadline } =
    case deadline of
        Left _ ->
            Sub.none

        Right _ ->
            decoderOutsideDeadline
                |> Decode.at [ "target", "id" ]
                |> Browser.Events.onClick


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
        , theme : Theme
    }
    -> Modal
    -> Element Msg
view ({ backdrop, theme } as model) (Modal modal) =
    Outside.view model
        { onClick = Exit
        , modal =
            column
                ([ width <| px 375
                 , height shrink
                 , padding 24
                 , spacing 16
                 , centerX
                 , centerY
                 , Border.rounded 8
                 , theme |> ThemeColor.border |> Border.color
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop theme
                )
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
                            , Font.size 16
                            , Font.bold
                            , paddingXY 0 3
                            , theme |> ThemeColor.text |> Font.color
                            ]
                            (text "Settings")
                        , IconButton.exit model Exit
                        ]
                    ]
                , slippageSetting modal theme
                , deadlineSetting modal theme
                , priceFeedSetting modal theme
                ]
        }


slippageSetting :
    { modal | slippage : Or Slippage String }
    -> Theme
    -> Element Msg
slippageSetting modal theme =
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
                , Font.size 14
                , theme |> ThemeColor.text |> Font.color
                ]
                (text "Slippage tolerance")
            ]
        , row
            [ width fill
            , height shrink
            , spacing 8
            ]
            [ slippageSwitch modal theme
            , slippageInput modal theme
            ]
        ]


deadlineSetting :
    { modal | deadline : Or Deadline String }
    -> Theme
    -> Element Msg
deadlineSetting modal theme =
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
                , Font.size 14
                , theme |> ThemeColor.text |> Font.color
                ]
                (text "Transaction deadline")
            ]
        , row
            [ width fill
            , height shrink
            , spacing 8
            ]
            [ deadlineSwitch modal theme
            , deadlineInput modal theme
            ]
        ]


priceFeedSetting : { modal | priceFeed : PriceFeed } -> Theme -> Element Msg
priceFeedSetting modal theme =
    row
        [ width fill
        , height fill
        , theme |> ThemeColor.text |> Font.color
        , Font.size 14
        ]
        [ text "CDP Spot Price"
        , priceFeedSwitch modal theme
        ]


slippageSwitch :
    { modal | slippage : Or Slippage String }
    -> Theme
    -> Element Msg
slippageSwitch { slippage } theme =
    Input.radioRow
        [ width shrink
        , height shrink
        , padding 4
        , spacing 4
        , theme |> ThemeColor.border |> Background.color
        , Border.rounded 8
        ]
        { onChange = ChooseSlippageOption
        , options =
            [ ( Slippage.Small, "0.1%" )
            , ( Slippage.Medium, "0.5%" )
            , ( Slippage.Large, "1%" )
            ]
                |> List.map
                    (\( options, label ) ->
                        radio label theme
                            |> Input.optionWith options
                    )
        , selected = slippage |> Slippage.toOption
        , label = Input.labelHidden "Slippage Switch"
        }


deadlineSwitch :
    { modal | deadline : Or Deadline String }
    -> Theme
    -> Element Msg
deadlineSwitch { deadline } theme =
    Input.radioRow
        [ width shrink
        , height shrink
        , padding 4
        , spacing 4
        , theme |> ThemeColor.border |> Background.color
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
                        radio label theme
                            |> Input.optionWith options
                    )
        , selected = deadline |> Deadline.toOption
        , label = Input.labelHidden "Deadline Switch"
        }


priceFeedSwitch :
    { modal | priceFeed : PriceFeed }
    -> Theme
    -> Element Msg
priceFeedSwitch { priceFeed } theme =
    el
        [ width <| px 40
        , height <| px 20
        , alignRight
        , centerY
        , padding 2
        , Border.rounded 500
        , pointer
        , onClick
            (ChoosePriceFeed
                (case priceFeed of
                    PriceFeed.Ignore ->
                        PriceFeed.Utilize

                    PriceFeed.Utilize ->
                        PriceFeed.Ignore
                )
            )
        , (case priceFeed of
            PriceFeed.Ignore ->
                theme |> ThemeColor.switchBG

            PriceFeed.Utilize ->
                theme |> ThemeColor.primaryBtn
          )
            |> Background.color
        ]
        (el
            [ width <| px 16
            , height <| px 16
            , centerY
            , Border.rounded 16
            , case priceFeed of
                PriceFeed.Ignore ->
                    alignLeft

                PriceFeed.Utilize ->
                    alignRight
            , (case priceFeed of
                PriceFeed.Ignore ->
                    Color.dark300

                PriceFeed.Utilize ->
                    Color.light100
              )
                |> Background.color
            ]
            none
        )


slippageInput :
    { modal | slippage : Or Slippage String }
    -> Theme
    -> Element Msg
slippageInput { slippage } theme =
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
        , (case slippage of
            Left _ ->
                theme |> ThemeColor.textboxBorder

            Right string ->
                if string |> Slippage.isCorrect then
                    theme |> ThemeColor.textboxBorder

                else
                    Color.negative500
          )
            |> Border.color
        , Border.rounded 8
        , Id.is "slippage"
        ]
        [ Input.text
            [ width fill
            , height shrink
            , centerY
            , Background.color Color.none
            , Border.color Color.none
            , Font.bold
            , Font.size 14
            , (case slippage of
                Left _ ->
                    theme |> ThemeColor.text

                Right string ->
                    if string |> Slippage.isCorrect then
                        theme |> ThemeColor.text

                    else
                        Color.negative500
              )
                |> Font.color
            ]
            { onChange = InputSlippage
            , text = slippage |> Slippage.toString
            , placeholder =
                Input.placeholder
                    [ (case slippage of
                        Left _ ->
                            theme |> ThemeColor.placeholder2

                        Right string ->
                            if string |> Slippage.isCorrect then
                                theme |> ThemeColor.placeholder2

                            else
                                Color.negative500
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
            , Font.size 14
            , (case slippage of
                Left _ ->
                    theme |> ThemeColor.text

                Right string ->
                    if string |> Slippage.isCorrect then
                        theme |> ThemeColor.text

                    else
                        Color.negative500
              )
                |> Font.color
            ]
            (text "%")
        ]


deadlineInput :
    { modal | deadline : Or Deadline String }
    -> Theme
    -> Element Msg
deadlineInput { deadline } theme =
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
        , (case deadline of
            Left _ ->
                theme |> ThemeColor.textboxBorder

            Right string ->
                if string |> Deadline.isCorrect then
                    theme |> ThemeColor.textboxBorder

                else
                    Color.negative500
          )
            |> Border.color
        , Border.rounded 8
        , Id.is "deadline"
        ]
        [ Input.text
            [ width fill
            , height shrink
            , centerY
            , Background.color Color.none
            , Border.color Color.none
            , Font.bold
            , Font.size 14
            , (case deadline of
                Left _ ->
                    theme |> ThemeColor.text

                Right string ->
                    if string |> Deadline.isCorrect then
                        theme |> ThemeColor.text

                    else
                        Color.negative500
              )
                |> Font.color
            ]
            { onChange = InputDeadline
            , text = deadline |> Deadline.toString
            , placeholder =
                Input.placeholder
                    [ (case deadline of
                        Left _ ->
                            theme |> ThemeColor.placeholder2

                        Right string ->
                            if string |> Deadline.isCorrect then
                                theme |> ThemeColor.placeholder2

                            else
                                Color.negative500
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
            , Font.size 14
            , (case deadline of
                Left _ ->
                    theme |> ThemeColor.text

                Right string ->
                    if string |> Deadline.isCorrect then
                        theme |> ThemeColor.text

                    else
                        Color.negative500
              )
                |> Font.color
            ]
            (text "m")
        ]


radio : String -> Theme -> OptionState -> Element msg
radio label theme optionState =
    el
        ([ width <| px 59
         , height <| px 36
         , Border.rounded 4
         ]
            ++ (case optionState of
                    Input.Selected ->
                        [ theme |> ThemeColor.primaryBtn |> Background.color ]

                    _ ->
                        []
               )
        )
        (el
            [ centerX
            , centerY
            , Font.size 16
            , Font.bold
            , (case optionState of
                Input.Selected ->
                    Color.transparent500

                _ ->
                    theme |> ThemeColor.textLight
              )
                |> Font.color
            ]
            (text label)
        )
