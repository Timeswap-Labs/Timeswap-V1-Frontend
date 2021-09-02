module Modals.Lend.ClaimsOut exposing (BondInput, ClaimsOut(..), InsuranceInput, SliderInput, hasFailure, updateAssetIn, updateEmptyAssetIn)

import Data.Balances as Balances exposing (Balances)
import Data.Images exposing (Images)
import Data.Pair as Pair exposing (Pair)
import Data.Percent as Percent exposing (Percent)
import Data.Remote exposing (Remote(..))
import Data.Uint as Uint
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , behindContent
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , newTabLink
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
import Element.Input as Input
import Utility.Color as Color
import Utility.Image as Image
import Utility.Loading as Loading


type ClaimsOut
    = Default (Remote Claims)
    | Slider SliderInput
    | Bond BondInput
    | Insurance InsuranceInput


type alias Claims =
    { bond : String
    , insurance : String
    }


type alias SliderInput =
    { percent : Percent
    , claims : Remote Claims
    }


type alias BondInput =
    { percent : Percent
    , bond : String
    , insurance : Remote String
    }


type alias InsuranceInput =
    { percent : Percent
    , bond : Remote String
    , insurance : String
    }


hasFailure : ClaimsOut -> Bool
hasFailure claimsOut =
    case claimsOut of
        Default Failure ->
            True

        Slider { claims } ->
            case claims of
                Failure ->
                    True

                _ ->
                    False

        Bond { insurance } ->
            case insurance of
                Failure ->
                    True

                _ ->
                    False

        Insurance { bond } ->
            case bond of
                Failure ->
                    True

                _ ->
                    False

        _ ->
            False


isAmount : { modal | pool : { pool | pair : Pair }, assetIn : String, claimsOut : ClaimsOut } -> Bool
isAmount { pool, assetIn, claimsOut } =
    (assetIn |> Uint.isAmount (pool.pair |> Pair.toAsset))
        && (case claimsOut of
                Default _ ->
                    True

                Slider _ ->
                    True

                Bond { bond } ->
                    bond |> Uint.isAmount (pool.pair |> Pair.toAsset)

                Insurance { insurance } ->
                    insurance |> Uint.isAmount (pool.pair |> Pair.toCollateral)
           )


hasBalanceIfUser :
    { model | user : Maybe { user | balances : Remote Balances } }
    -> { modal | pool : { pool | pair : Pair }, assetIn : String }
    -> Bool
hasBalanceIfUser { user } { pool, assetIn } =
    user
        |> Maybe.map
            (\{ balances } ->
                case balances of
                    Loading ->
                        True

                    Failure ->
                        False

                    Success successBalances ->
                        successBalances
                            |> Balances.hasEnough (pool.pair |> Pair.toAsset) assetIn
            )
        |> Maybe.withDefault True


isCorrect :
    { model | user : Maybe { user | balances : Balances } }
    -> { modal | pool : { pool | pair : Pair }, assetIn : String, claimsOut : ClaimsOut }
    -> Bool
isCorrect { user } { pool, assetIn, claimsOut } =
    True


updateAssetIn : ClaimsOut -> ClaimsOut
updateAssetIn claimsOut =
    case claimsOut of
        Default _ ->
            Default Loading

        Slider sliderInput ->
            { sliderInput | claims = Loading }
                |> Slider

        Bond bondInput ->
            { bondInput | insurance = Loading }
                |> Bond

        Insurance insuranceInput ->
            { insuranceInput | bond = Loading }
                |> Insurance


updateEmptyAssetIn : ClaimsOut -> ClaimsOut
updateEmptyAssetIn claimsOut =
    case claimsOut of
        Default _ ->
            Success
                { bond = ""
                , insurance = ""
                }
                |> Default

        Slider sliderInput ->
            { sliderInput
                | claims =
                    Success
                        { bond = ""
                        , insurance = ""
                        }
            }
                |> Slider

        Bond bondInput ->
            { bondInput | insurance = Success "" }
                |> Bond

        Insurance insuranceInput ->
            { insuranceInput | bond = Success "" }
                |> Insurance


view :
    { msgs | switchLendSetting : Bool -> msg, slide : Float -> msg }
    -> { model | images : Images }
    -> { modal | claimsOut : ClaimsOut, apr : Remote String }
    -> Element msg
view msgs model modal =
    column
        [ width fill
        , height shrink
        ]
        [ title msgs model modal
        , position msgs modal

        -- , timeToMaturity model lendModel
        ]


title :
    { msgs | switchLendSetting : Bool -> msg }
    -> { model | images : Images }
    -> { modal | claimsOut : ClaimsOut }
    -> Element msg
title msgs { images } modal =
    row
        [ width fill
        , height shrink
        , paddingXY 20 12
        , spacing 9
        , Border.width 1
        , Border.solid
        , Border.color Color.transparent100
        , Border.roundEach
            { topLeft = 4
            , topRight = 4
            , bottomRight = 0
            , bottomLeft = 0
            }
        , Font.size 14
        ]
        [ el
            [ alignLeft
            , paddingXY 0 4
            , Font.size 16
            , Font.bold
            , Font.color Color.transparent500
            ]
            (text "Your Position")
        , Image.info images
            [ width <| px 24
            , alignLeft
            ]
        , el
            [ alignRight
            , centerY
            , paddingXY 0 3
            , Font.size 14
            , Font.regular
            , Font.color Color.transparent300
            ]
            (text "Customize Risk")
        , switch msgs modal
        ]


switch :
    { msgs | switchLendSetting : Bool -> msg }
    -> { modal | claimsOut : ClaimsOut }
    -> Element msg
switch msgs { claimsOut } =
    Input.checkbox
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        ]
        { onChange = msgs.switchLendSetting
        , icon =
            \checked ->
                if checked then
                    el
                        [ width <| px 40
                        , height <| px 20
                        , padding 2
                        , Background.color Color.primary400
                        , Border.rounded 999
                        ]
                        (el
                            [ width <| px 16
                            , height <| px 16
                            , alignRight
                            , Background.color Color.light100
                            , Border.rounded 999
                            ]
                            none
                        )

                else
                    el
                        [ width <| px 40
                        , height <| px 20
                        , padding 2
                        , Background.color Color.transparent200
                        , Border.rounded 999
                        ]
                        (el
                            [ width <| px 16
                            , height <| px 16
                            , alignLeft
                            , Background.color Color.dark300
                            , Border.rounded 999
                            ]
                            none
                        )
        , checked =
            case claimsOut of
                Default _ ->
                    False

                _ ->
                    True
        , label = Input.labelHidden "SwitchSetting"
        }


position :
    { msgs | slide : Float -> msg }
    -> { modal | claimsOut : ClaimsOut, apr : Remote String }
    -> Element msg
position msgs ({ claimsOut } as modal) =
    column
        [ width fill
        , height shrink
        , padding 20
        , spacing 12
        , Border.widthEach
            { top = 0
            , right = 1
            , bottom = 1
            , left = 1
            }
        , Border.solid
        , Border.color Color.transparent100
        ]
        [ (case claimsOut of
            Slider { percent } ->
                Just percent

            Bond { percent } ->
                Just percent

            Insurance { percent } ->
                Just percent

            _ ->
                Nothing
          )
            |> Maybe.map (sliderSection msgs modal)
            |> Maybe.withDefault none
        , estimatedAPR modal

        --, claimsOut lendModel
        ]


sliderSection :
    { msgs | slide : Float -> msg }
    -> { modal | claimsOut : ClaimsOut }
    -> Percent
    -> Element msg
sliderSection msgs modal percent =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ row
            [ width fill
            , height shrink
            , paddingXY 0 3
            , Font.size 14
            ]
            [ el
                [ alignLeft
                , Font.regular
                , Font.color Color.transparent500
                ]
                (text "Adjust your risk")
            , newTabLink
                [ alignRight
                , Font.regular
                , Font.color Color.primary500
                ]
                { url = "https://timeswap.gitbook.io/timeswap/deep-dive/lending"
                , label = text "Learn more"
                }
            ]
        , column
            [ width fill
            , height shrink
            , spacing 6
            ]
            [ slider msgs modal percent
            , row
                [ width fill
                , height shrink
                , paddingXY 0 2
                , Font.size 12
                , Font.color Color.transparent300
                ]
                [ el [ alignLeft, Font.regular ] (text "Low")
                , el [ alignRight, Font.regular ] (text "High")
                ]
            ]
        ]


slider :
    { msgs | slide : Float -> msg }
    -> { modal | claimsOut : ClaimsOut }
    -> Percent
    -> Element msg
slider msgs { claimsOut } percent =
    Input.slider
        [ width fill
        , el
            [ width fill
            , height <| px 2
            , centerY
            , (if claimsOut |> hasFailure then
                Color.negative500

               else
                Color.transparent100
              )
                |> Background.color
            ]
            none
            |> behindContent
        ]
        { onChange = msgs.slide
        , label = Input.labelHidden "Slider"
        , min = 0
        , max = 128
        , value = percent |> Percent.toFloat
        , thumb =
            Input.thumb
                [ width <| px 20
                , height <| px 20
                , Background.color Color.primary500
                , Border.rounded 999
                , Border.width 2
                , Border.color Color.transparent500
                ]
        , step = Just 1
        }


estimatedAPR : { modal | claimsOut : ClaimsOut, apr : Remote String } -> Element msg
estimatedAPR { claimsOut, apr } =
    row
        [ width shrink
        , height <| px 32
        , paddingXY 12 0
        , spacing 5
        , centerX
        , Background.color Color.primary100
        , Border.rounded 20
        ]
        [ el
            [ width shrink
            , height shrink
            , paddingXY 0 3
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.transparent400
            ]
            (text "Estimated APR")
        , el
            [ width shrink
            , height shrink
            , paddingXY 0 3
            , centerY
            , Font.bold
            , Font.size 18
            , (if claimsOut |> hasFailure then
                Color.negative500

               else
                Color.positive400
              )
                |> Font.color
            ]
            (case apr of
                Loading ->
                    Loading.view

                Failure ->
                    none

                Success successAPR ->
                    text (successAPR ++ "%")
            )
        ]
