module Modals.Lend.ClaimsOut exposing
    ( BondInput
    , ClaimsOut(..)
    , InsuranceInput
    , SliderInput
    , hasFailure
    , hasZeroInput
    , isCorrect
    , isDefault
    , slide
    , slideZero
    , switchLendSetting
    , switchLendSettingZero
    , updateAssetIn
    , updateAssetInZero
    , updateBondOut
    , updateBondOutZero
    , updateInsuranceOut
    , updateInsuranceOutZero
    , view
    )

import Data.Balances as Balances exposing (Balances)
import Data.Images exposing (Images)
import Data.Pair as Pair exposing (Pair)
import Data.Percent as Percent exposing (Percent)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenImages exposing (TokenImages)
import Data.Uint as Uint
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , alpha
        , behindContent
        , centerX
        , centerY
        , clip
        , column
        , el
        , fill
        , height
        , inFront
        , newTabLink
        , none
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
import Utility.Color as Color
import Utility.Image as Image
import Utility.Input as Input
import Utility.Loading as Loading
import Utility.TokenImage as TokenImage


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


hasFailure : { modal | claimsOut : ClaimsOut } -> Bool
hasFailure { claimsOut } =
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
    { model | user : Maybe { user | balances : Remote Balances } }
    -> { modal | pool : { pool | pair : Pair }, assetIn : String, claimsOut : ClaimsOut }
    -> Bool
isCorrect model modal =
    (hasFailure modal |> not)
        && isAmount modal
        && hasBalanceIfUser model modal


hasZeroInput : ClaimsOut -> Bool
hasZeroInput claimsOut =
    case claimsOut of
        Bond { bond } ->
            bond |> Input.isZero

        Insurance { insurance } ->
            insurance |> Input.isZero

        _ ->
            False


isDefault : { modal | claimsOut : ClaimsOut } -> Bool
isDefault { claimsOut } =
    case claimsOut of
        Default _ ->
            True

        Slider { percent } ->
            (percent |> Percent.toFloat) == 64

        _ ->
            False


updateAssetInZero : ClaimsOut -> ClaimsOut
updateAssetInZero claimsOut =
    case claimsOut of
        Default _ ->
            { bond = ""
            , insurance = ""
            }
                |> Success
                |> Default

        Slider sliderInput ->
            { sliderInput
                | claims =
                    { bond = ""
                    , insurance = ""
                    }
                        |> Success
            }
                |> Slider

        Bond bondInput ->
            { bondInput | insurance = Success "" }
                |> Bond

        Insurance insuranceInput ->
            { insuranceInput | bond = Success "" }
                |> Insurance


updateAssetIn : ClaimsOut -> ClaimsOut
updateAssetIn claimsOut =
    case claimsOut of
        Default _ ->
            Default Loading

        Slider sliderInput ->
            { sliderInput | claims = Loading }
                |> Slider

        Bond ({ bond } as bondInput) ->
            { bondInput
                | insurance =
                    if bond |> Input.isZero then
                        Success ""

                    else
                        Loading
            }
                |> Bond

        Insurance ({ insurance } as insuranceInput) ->
            { insuranceInput
                | bond =
                    if insurance |> Input.isZero then
                        Success ""

                    else
                        Loading
            }
                |> Insurance


switchLendSettingZero : Bool -> ClaimsOut -> ClaimsOut
switchLendSettingZero checked claimsOut =
    if checked then
        case claimsOut of
            Default _ ->
                { percent = Percent.init
                , claims =
                    { bond = ""
                    , insurance = ""
                    }
                        |> Success
                }
                    |> Slider

            _ ->
                claimsOut

    else
        { bond = ""
        , insurance = ""
        }
            |> Success
            |> Default


switchLendSetting : Bool -> ClaimsOut -> ClaimsOut
switchLendSetting checked claimsOut =
    if checked then
        case claimsOut of
            Default claims ->
                Slider
                    { percent = Percent.init
                    , claims = claims
                    }

            _ ->
                claimsOut

    else
        case claimsOut of
            Slider { percent, claims } ->
                if (percent |> Percent.toFloat) == 64 then
                    Default claims

                else
                    Default Loading

            Default _ ->
                claimsOut

            _ ->
                Default Loading


slideZero : Float -> ClaimsOut
slideZero float =
    { percent = float |> Percent.fromFloat
    , claims =
        { bond = ""
        , insurance = ""
        }
            |> Success
    }
        |> Slider


slide : Float -> ClaimsOut
slide float =
    { percent = float |> Percent.fromFloat
    , claims = Loading
    }
        |> Slider


updateBondOutZero : String -> ClaimsOut -> ClaimsOut
updateBondOutZero string claimsOut =
    (case claimsOut of
        Slider { percent } ->
            Just percent

        Bond { percent } ->
            Just percent

        Insurance { percent } ->
            Just percent

        _ ->
            Nothing
    )
        |> Maybe.map
            (\percent ->
                { percent = percent
                , bond = string
                , insurance = Success ""
                }
                    |> Bond
            )
        |> Maybe.withDefault claimsOut


updateBondOut : String -> ClaimsOut -> ClaimsOut
updateBondOut string claimsOut =
    (case claimsOut of
        Slider { percent } ->
            Just percent

        Bond { percent } ->
            Just percent

        Insurance { percent } ->
            Just percent

        _ ->
            Nothing
    )
        |> Maybe.map
            (\percent ->
                { percent = percent
                , bond = string
                , insurance =
                    if string |> Input.isZero then
                        Success ""

                    else
                        Loading
                }
                    |> Bond
            )
        |> Maybe.withDefault claimsOut


updateInsuranceOutZero : String -> ClaimsOut -> ClaimsOut
updateInsuranceOutZero string claimsOut =
    (case claimsOut of
        Slider { percent } ->
            Just percent

        Bond { percent } ->
            Just percent

        Insurance { percent } ->
            Just percent

        _ ->
            Nothing
    )
        |> Maybe.map
            (\percent ->
                { percent = percent
                , bond = Success ""
                , insurance = string
                }
                    |> Insurance
            )
        |> Maybe.withDefault claimsOut


updateInsuranceOut : String -> ClaimsOut -> ClaimsOut
updateInsuranceOut string claimsOut =
    (case claimsOut of
        Slider { percent } ->
            Just percent

        Bond { percent } ->
            Just percent

        Insurance { percent } ->
            Just percent

        _ ->
            Nothing
    )
        |> Maybe.map
            (\percent ->
                { percent = percent
                , bond =
                    if string |> Input.isZero then
                        Success ""

                    else
                        Loading
                , insurance =
                    string
                }
                    |> Insurance
            )
        |> Maybe.withDefault claimsOut


view :
    { msgs
        | switchLendSetting : Bool -> msg
        , slide : Float -> msg
        , inputBondOut : String -> msg
        , inputInsuranceOut : String -> msg
    }
    ->
        { model
            | images : Images
            , tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
            , apr : Remote String
            , cf : Remote String
        }
    -> Element msg
view msgs model modal =
    column
        [ width fill
        , height shrink
        ]
        [ title msgs model modal
        , position msgs model modal

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
            [ width <| px 16
            , alignLeft
            , centerY
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
    { msgs
        | slide : Float -> msg
        , inputBondOut : String -> msg
        , inputInsuranceOut : String -> msg
    }
    ->
        { model
            | images : Images
            , tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
            , apr : Remote String
            , cf : Remote String
        }
    -> Element msg
position msgs model ({ claimsOut } as modal) =
    column
        [ width fill
        , height shrink
        , padding 20
        , spacing 16
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
            |> Maybe.map (sliderSection msgs model modal)
            |> Maybe.withDefault none
        , column
            [ width fill
            , height shrink
            , spacing 12
            ]
            [ estimatedAPR modal
            , collateralFactor modal
            ]
        , bondOutSection msgs model modal
        , insuranceOutSection msgs model modal
        ]


sliderSection :
    { msgs | slide : Float -> msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    -> { modal | pool : { pool | pair : Pair }, assetIn : String, claimsOut : ClaimsOut }
    -> Percent
    -> Element msg
sliderSection msgs model modal percent =
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
            [ slider msgs model modal percent
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
    -> { model | user : Maybe { user | balances : Remote Balances } }
    -> { modal | pool : { pool | pair : Pair }, assetIn : String, claimsOut : ClaimsOut }
    -> Percent
    -> Element msg
slider msgs model modal percent =
    Input.slider
        [ width fill
        , el
            [ width fill
            , height <| px 2
            , centerY
            , (if isCorrect model modal then
                Color.transparent100

               else
                Color.negative500
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


estimatedAPR :
    { modal | apr : Remote String }
    -> Element msg
estimatedAPR { apr } =
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
            (text "Estimated APR:")
        , el
            [ width shrink
            , height shrink
            , paddingXY 0 3
            , centerY
            , Font.bold
            , Font.size 18
            , Font.color Color.positive500
            ]
            (case apr of
                Loading ->
                    el
                        [ width <| px 50
                        , height shrink
                        ]
                        Loading.view

                Failure ->
                    none

                Success successAPR ->
                    text successAPR
            )
        , el
            [ paddingXY 4 0
            , centerY
            , Font.bold
            , Font.size 18
            , Font.color Color.positive500
            ]
            (text "%")
        ]


collateralFactor :
    { modal
        | pool : { pool | pair : Pair }
        , assetIn : String
        , claimsOut : ClaimsOut
        , cf : Remote String
    }
    -> Element msg
collateralFactor { pool, cf } =
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
            (text "Collateral factor:")
        , el
            [ width shrink
            , height shrink
            , paddingXY 0 3
            , centerY
            , Font.bold
            , Font.size 18
            , Font.color Color.transparent500
            ]
            (case cf of
                Loading ->
                    el
                        [ width <| px 50
                        , height shrink
                        ]
                        Loading.view

                Failure ->
                    none

                Success successCF ->
                    text successCF
            )
        , el
            [ paddingXY 4 0
            , centerY
            , Font.bold
            , Font.size 12
            , Font.color Color.transparent300
            ]
            ([ pool.pair
                |> Pair.toAsset
                |> Token.toSymbol
             , "PER"
             , pool.pair
                |> Pair.toCollateral
                |> Token.toSymbol
             ]
                |> String.join " "
                |> text
            )
        ]


bondOutSection :
    { msgs | inputBondOut : String -> msg }
    ->
        { model
            | images : Images
            , tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
bondOutSection msgs ({ images } as model) modal =
    column
        [ width fill
        , height shrink
        , spacing 12
        , alignLeft
        ]
        [ row
            [ width shrink
            , height shrink
            , spacing 8
            ]
            [ el
                [ width shrink
                , height shrink
                , paddingXY 0 3
                , alignLeft
                , Font.regular
                , Font.size 14
                , Font.color Color.transparent400
                ]
                (text "Amount at maturity")
            , Image.info images
                [ width <| px 16
                , centerY
                ]
            ]
        , bondOutTextbox msgs model modal
        ]


bondOutTextbox :
    { msgs | inputBondOut : String -> msg }
    ->
        { model
            | tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
bondOutTextbox msgs model modal =
    row
        [ width fill
        , height <| px 44
        ]
        [ bondOutLogo model modal
        , bondOutAmount msgs model modal
        ]


bondOutLogo :
    { model
        | tokenImages : TokenImages
        , user : Maybe { user | balances : Remote Balances }
    }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
bondOutLogo ({ tokenImages } as model) ({ pool } as modal) =
    row
        [ width shrink
        , height fill
        , paddingXY 12 0
        , spacing 6
        , Background.color Color.primary100
        , Border.widthEach
            { top = 1
            , right = 0
            , bottom = 1
            , left = 1
            }
        , Border.solid
        , (if isCorrect model modal then
            Color.transparent100

           else
            Color.negative500
          )
            |> Border.color
        , Border.roundEach
            { topLeft = 4
            , topRight = 0
            , bottomRight = 0
            , bottomLeft = 4
            }
        ]
        [ pool.pair
            |> Pair.toAsset
            |> TokenImage.icon tokenImages
                [ width <| px 24
                , alignLeft
                , centerY
                ]
        , el
            [ alignLeft
            , centerY
            , Font.regular
            , Font.size 16
            , Font.color Color.light100
            ]
            (pool.pair
                |> Pair.toAsset
                |> Token.toSymbol
                |> text
            )
        ]


bondOutAmount :
    { msgs | inputBondOut : String -> msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
bondOutAmount msgs model ({ claimsOut } as modal) =
    el
        ([ width fill
         , height fill
         , paddingEach
            { top = 0
            , right = 12
            , bottom = 0
            , left = 0
            }
         , spacing 8
         , Border.widthEach
            { top = 1
            , right = 1
            , bottom = 1
            , left = 0
            }
         , Border.solid
         , (if isCorrect model modal then
                Color.transparent100

            else
                Color.negative500
           )
            |> Border.color
         , Border.roundEach
            { topLeft = 0
            , topRight = 4
            , bottomRight = 4
            , bottomLeft = 0
            }
         ]
            ++ (case claimsOut of
                    Default _ ->
                        [ Background.color Color.primary100
                        , alpha 0.75
                        ]

                    _ ->
                        []
               )
        )
        (bondOutInput msgs model modal)


bondOutInput :
    { msgs | inputBondOut : String -> msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
bondOutInput msgs model ({ claimsOut } as modal) =
    (\maybeBondOut ->
        row
            [ width fill
            , height fill
            , paddingXY 12 0
            , spacing 12
            ]
            [ Input.text
                [ width fill
                , height shrink
                , paddingXY 0 4
                , alignLeft
                , centerY
                , Background.color Color.none
                , Border.color Color.none
                , Font.regular
                , Font.size 16
                , (if isCorrect model modal then
                    Color.transparent500

                   else
                    Color.negative500
                  )
                    |> Font.color
                ]
                { onChange = msgs.inputBondOut
                , text = maybeBondOut |> Maybe.withDefault ""
                , placeholder =
                    Input.placeholder
                        [ Font.color Color.transparent100 ]
                        (text "0.0")
                        |> Just
                , label = Input.labelHidden "Bond Amount"
                }
            , maybeBondOut
                |> Maybe.map (\_ -> none)
                |> Maybe.withDefault
                    (el
                        [ width <| px 50
                        , height shrink
                        , alignRight
                        , centerY
                        ]
                        Loading.view
                    )
            ]
    )
        |> (\element ->
                case claimsOut of
                    Default claims ->
                        row
                            [ width fill
                            , height shrink
                            , paddingXY 12 0
                            , spacing 12
                            , alignLeft
                            , centerY
                            , Font.regular
                            , Font.size 16
                            ]
                            (case claims of
                                Loading ->
                                    [ el
                                        [ paddingXY 0 4
                                        , centerY
                                        , Font.color Color.transparent100
                                        ]
                                        (text "0.0")
                                    , el
                                        [ width <| px 50
                                        , height shrink
                                        , alignRight
                                        , centerY
                                        ]
                                        Loading.view
                                    ]

                                Failure ->
                                    [ el
                                        [ paddingXY 0 4
                                        , centerY
                                        , Font.color Color.transparent100
                                        ]
                                        (text "0.0")
                                    ]

                                Success { bond } ->
                                    [ el
                                        [ paddingXY 0 4
                                        , centerY
                                        , clip
                                        , (if isCorrect model modal then
                                            Color.transparent500

                                           else
                                            Color.negative500
                                          )
                                            |> Font.color
                                        ]
                                        (text bond)
                                    ]
                            )

                    Slider { claims } ->
                        case claims of
                            Loading ->
                                element Nothing

                            Failure ->
                                element (Just "")

                            Success { bond } ->
                                element (Just bond)

                    Bond { bond } ->
                        element (Just bond)

                    Insurance { bond } ->
                        case bond of
                            Loading ->
                                element Nothing

                            Failure ->
                                element (Just "")

                            Success successBond ->
                                element (Just successBond)
           )


insuranceOutSection :
    { msgs | inputInsuranceOut : String -> msg }
    ->
        { model
            | images : Images
            , tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
insuranceOutSection msgs ({ images } as model) modal =
    column
        [ width fill
        , height shrink
        , spacing 12
        , alignLeft
        ]
        [ row
            [ width shrink
            , height shrink
            , spacing 8
            ]
            [ el
                [ width shrink
                , height shrink
                , paddingXY 0 3
                , alignLeft
                , Font.regular
                , Font.size 14
                , Font.color Color.transparent400
                ]
                (text "Insurance in case of default")
            , Image.info images
                [ width <| px 16
                , centerY
                ]
            ]
        , insuranceOutTextbox msgs model modal
        ]


insuranceOutTextbox :
    { msgs | inputInsuranceOut : String -> msg }
    ->
        { model
            | tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
insuranceOutTextbox msgs model modal =
    row
        [ width fill
        , height <| px 44
        ]
        [ insuranceOutLogo model modal
        , insuranceOutAmount msgs model modal
        ]


insuranceOutLogo :
    { model
        | tokenImages : TokenImages
        , user : Maybe { user | balances : Remote Balances }
    }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
insuranceOutLogo ({ tokenImages } as model) ({ pool } as modal) =
    row
        [ width shrink
        , height fill
        , paddingXY 12 0
        , spacing 6
        , Background.color Color.primary100
        , Border.widthEach
            { top = 1
            , right = 0
            , bottom = 1
            , left = 1
            }
        , Border.solid
        , (if isCorrect model modal then
            Color.transparent100

           else
            Color.negative500
          )
            |> Border.color
        , Border.roundEach
            { topLeft = 4
            , topRight = 0
            , bottomRight = 0
            , bottomLeft = 4
            }
        ]
        [ pool.pair
            |> Pair.toCollateral
            |> TokenImage.icon tokenImages
                [ width <| px 24
                , alignLeft
                , centerY
                ]
        , el
            [ alignLeft
            , centerY
            , Font.regular
            , Font.size 16
            , Font.color Color.light100
            ]
            (pool.pair
                |> Pair.toCollateral
                |> Token.toSymbol
                |> text
            )
        ]


insuranceOutAmount :
    { msgs | inputInsuranceOut : String -> msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
insuranceOutAmount msgs model ({ claimsOut } as modal) =
    el
        ([ width fill
         , height fill
         , paddingEach
            { top = 0
            , right = 12
            , bottom = 0
            , left = 0
            }
         , spacing 8
         , Border.widthEach
            { top = 1
            , right = 1
            , bottom = 1
            , left = 0
            }
         , Border.solid
         , (if isCorrect model modal then
                Color.transparent100

            else
                Color.negative500
           )
            |> Border.color
         , Border.roundEach
            { topLeft = 0
            , topRight = 4
            , bottomRight = 4
            , bottomLeft = 0
            }
         ]
            ++ (case claimsOut of
                    Default _ ->
                        [ Background.color Color.primary100
                        , alpha 0.75
                        ]

                    _ ->
                        []
               )
        )
        (insuranceOutInput msgs model modal)


insuranceOutInput :
    { msgs | inputInsuranceOut : String -> msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
insuranceOutInput msgs model ({ claimsOut } as modal) =
    (\maybeInsuranceOut ->
        row
            [ width fill
            , height fill
            , paddingXY 12 0
            , spacing 12
            ]
            [ Input.text
                [ width fill
                , height shrink
                , paddingXY 0 4
                , alignLeft
                , centerY
                , Background.color Color.none
                , Border.color Color.none
                , Font.regular
                , Font.size 16
                , (if isCorrect model modal then
                    Color.transparent500

                   else
                    Color.negative500
                  )
                    |> Font.color
                ]
                { onChange = msgs.inputInsuranceOut
                , text = maybeInsuranceOut |> Maybe.withDefault ""
                , placeholder =
                    Input.placeholder
                        [ Font.color Color.transparent100 ]
                        (text "0.0")
                        |> Just
                , label = Input.labelHidden "Insurance Amount"
                }
            , maybeInsuranceOut
                |> Maybe.map (\_ -> none)
                |> Maybe.withDefault
                    (el
                        [ width <| px 50
                        , height shrink
                        , alignRight
                        , centerY
                        ]
                        Loading.view
                    )
            ]
    )
        |> (\element ->
                case claimsOut of
                    Default claims ->
                        row
                            [ width fill
                            , height shrink
                            , paddingXY 12 0
                            , spacing 12
                            , alignLeft
                            , centerY
                            , Font.regular
                            , Font.size 16
                            ]
                            (case claims of
                                Loading ->
                                    [ el
                                        [ paddingXY 0 4
                                        , centerY
                                        , Font.color Color.transparent100
                                        ]
                                        (text "0.0")
                                    , el
                                        [ width <| px 50
                                        , height shrink
                                        , alignRight
                                        , centerY
                                        ]
                                        Loading.view
                                    ]

                                Failure ->
                                    [ el
                                        [ paddingXY 0 4
                                        , centerY
                                        , Font.color Color.transparent100
                                        ]
                                        (text "0.0")
                                    ]

                                Success { insurance } ->
                                    [ el
                                        [ paddingXY 0 4
                                        , centerY
                                        , clip
                                        , (if isCorrect model modal then
                                            Color.transparent500

                                           else
                                            Color.negative500
                                          )
                                            |> Font.color
                                        ]
                                        (text insurance)
                                    ]
                            )

                    Slider { claims } ->
                        case claims of
                            Loading ->
                                element Nothing

                            Failure ->
                                element (Just "")

                            Success { insurance } ->
                                element (Just insurance)

                    Bond { insurance } ->
                        case insurance of
                            Loading ->
                                element Nothing

                            Failure ->
                                element (Just "")

                            Success successInsurance ->
                                element (Just successInsurance)

                    Insurance { insurance } ->
                        element (Just insurance)
           )
