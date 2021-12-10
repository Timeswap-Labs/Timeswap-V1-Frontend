module Page.Transaction.Lend.Lend.Disabled exposing
    ( BondInput
    , ClaimsOut(..)
    , InsuranceInput
    , Transaction
    , init
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Data.Images exposing (Images)
import Data.Pair as Pair exposing (Pair)
import Data.Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.Token exposing (Token)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , alpha
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
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
import Element.Region as Region
import Page.Transaction.Info as Info
import Page.Transaction.Output as Output
import Page.Transaction.Slider as Slider
import Page.Transaction.Switch as Switch
import Page.Transaction.Textbox as Textbox
import Utility.Color as Color
import Utility.Truncate as Truncate


type alias Transaction =
    { assetIn : String
    , claimsOut : ClaimsOut
    }


type ClaimsOut
    = Default
    | Slider Percent
    | Bond BondInput
    | Insurance InsuranceInput


type alias BondInput =
    { percent : Percent
    , bondOut : String
    }


type alias InsuranceInput =
    { percent : Percent
    , insuranceOut : String
    }


init : Transaction
init =
    { assetIn = ""
    , claimsOut = Default
    }


view :
    { model | images : Images }
    -> Blockchain
    -> Pool
    -> Transaction
    ->
        { first : Element Never
        , second : Element Never
        }
view model blockchain pool transaction =
    { first =
        transaction
            |> assetInSection
                model
                blockchain
                (pool.pair |> Pair.toAsset)
    , second =
        transaction
            |> disabledClaims model pool
    }


assetInSection :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | assetIn : String }
    -> Element Never
assetInSection model blockchain asset transaction =
    column
        [ Region.description "lend asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 6
            , centerY
            ]
            (el
                [ width shrink
                , height shrink
                , Font.size 14
                , paddingXY 0 3
                , Font.color Color.primary400
                ]
                (text "Amount to Lend")
                :: (blockchain
                        |> Blockchain.toUser
                        |> Maybe.map
                            (\user ->
                                [ userBalance user asset
                                , maxButton
                                ]
                            )
                        |> Maybe.withDefault
                            []
                   )
            )
        , Textbox.disabled model
            { token = asset
            , text = transaction.assetIn
            , description = "lend asset textbox"
            }
        ]


userBalance :
    User
    -> Token
    -> Element Never
userBalance user asset =
    user
        |> User.getBalance asset
        |> Maybe.map
            (\balance ->
                row
                    [ width shrink
                    , height shrink
                    , alignRight
                    , centerY
                    ]
                    [ el
                        [ width shrink
                        , height shrink
                        , Font.size 12
                        , paddingXY 0 2
                        , Font.color Color.transparent300
                        ]
                        (text "Bal: ")
                    , Truncate.disabledBalance
                        { token = asset
                        , balance = balance
                        }
                    ]
            )
        |> Maybe.withDefault none


maxButton : Element Never
maxButton =
    el
        [ Region.description "max asset lend"
        , width shrink
        , height shrink
        , alignRight
        , centerY
        , Font.size 12
        , paddingXY 0 2
        , Font.color Color.warning400
        , Font.bold
        ]
        (text "MAX")


disabledClaims :
    { model | images : Images }
    -> Pool
    -> { transaction | claimsOut : ClaimsOut }
    -> Element Never
disabledClaims model pool ({ claimsOut } as transaction) =
    column
        [ Region.description "claims"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ (case claimsOut of
            Default ->
                Switch.Recommended

            _ ->
                Switch.Advanced
          )
            |> Switch.disabled
        , (case claimsOut of
            Default ->
                Nothing

            Slider percent ->
                Just percent

            Bond { percent } ->
                Just percent

            Insurance { percent } ->
                Just percent
          )
            |> Maybe.map disabledSlider
            |> Maybe.withDefault none
        , row
            [ width fill
            , height shrink
            , spacing 16
            ]
            [ Info.emptyAPR
            , Info.emptyCDP
            ]
        , disabledClaimsOut model pool.pair transaction
        ]


disabledSlider : Percent -> Element Never
disabledSlider percent =
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
                (text "Adjust your APR")
            ]
        , column
            [ width fill
            , height shrink
            , spacing 6
            ]
            [ Slider.disabled percent
            , row
                [ width fill
                , height shrink
                , paddingXY 0 2
                , Font.size 12
                , Font.color Color.transparent300
                ]
                [ el
                    [ alignLeft
                    , Font.regular
                    ]
                    (text "Low")
                , el
                    [ alignRight
                    , Font.regular
                    ]
                    (text "High")
                ]
            ]
        ]


disabledClaimsOut :
    { model | images : Images }
    -> Pair
    -> { transaction | claimsOut : ClaimsOut }
    -> Element Never
disabledClaimsOut model pair { claimsOut } =
    case claimsOut of
        Default ->
            column
                [ width fill
                , height shrink
                , padding 12
                , spacing 12
                , Background.color Color.primary100
                , Border.rounded 8
                ]
                [ pair
                    |> Pair.toAsset
                    |> Just
                    |> bondOutSection model
                , pair
                    |> Pair.toCollateral
                    |> Just
                    |> insuranceOutSection model
                ]

        Slider _ ->
            column
                [ width fill
                , height shrink
                , spacing 12
                ]
                [ Nothing
                    |> disabledBondOut model
                        (pair |> Pair.toAsset)
                , Nothing
                    |> disabledInsuranceOut model
                        (pair |> Pair.toCollateral)
                ]

        Bond { bondOut } ->
            column
                [ width fill
                , height shrink
                , spacing 12
                ]
                [ bondOut
                    |> Just
                    |> disabledBondOut model
                        (pair |> Pair.toAsset)
                , Nothing
                    |> disabledInsuranceOut model
                        (pair |> Pair.toCollateral)
                ]

        Insurance { insuranceOut } ->
            column
                [ width fill
                , height shrink
                , spacing 12
                ]
                [ Nothing
                    |> disabledBondOut model
                        (pair |> Pair.toAsset)
                , insuranceOut
                    |> Just
                    |> disabledInsuranceOut model
                        (pair |> Pair.toCollateral)
                ]


bondOutSection :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
bondOutSection model asset =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Amount to Receive")
        , asset
            |> Maybe.map
                (\token ->
                    Output.disabled model
                        { token = token
                        , description = "bond output"
                        }
                )
            |> Maybe.withDefault
                (el
                    [ width fill
                    , height <| px 24
                    ]
                    none
                )
        ]


insuranceOutSection :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
insuranceOutSection model collateral =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Amount Protecting")
        , collateral
            |> Maybe.map
                (\token ->
                    Output.disabled model
                        { token = token
                        , description = "insurance output"
                        }
                )
            |> Maybe.withDefault
                (el
                    [ width fill
                    , height <| px 24
                    ]
                    none
                )
        ]


disabledBondOut :
    { model | images : Images }
    -> Token
    -> Maybe String
    -> Element Never
disabledBondOut model asset input =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Amount to Receive")
        , Textbox.disabled model
            { token = asset
            , text = input |> Maybe.withDefault ""
            , description = "bond out input"
            }
        ]


disabledInsuranceOut :
    { model | images : Images }
    -> Token
    -> Maybe String
    -> Element Never
disabledInsuranceOut model collateral input =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Amount Protecting")
        , Textbox.disabled model
            { token = collateral
            , text = input |> Maybe.withDefault ""
            , description = "insurance out input"
            }
        ]
