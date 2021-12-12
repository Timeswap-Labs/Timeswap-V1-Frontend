module Page.Transaction.Lend.Lend.Disabled exposing
    ( BondInput
    , ClaimsOut(..)
    , InsuranceInput
    , Transaction
    , init
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Data.Images exposing (Images)
import Data.Mode as Mode exposing (Mode)
import Data.Pair as Pair
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
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Output as Output
import Page.Transaction.Slider as Slider
import Page.Transaction.Switch as Switch
import Page.Transaction.Textbox as Textbox
import Utility.Color as Color


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
            |> claimsOutSection model pool
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
            [ el
                [ width shrink
                , height shrink
                , Font.size 14
                , paddingXY 0 3
                , Font.color Color.primary400
                ]
                (text "Amount to Lend")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance asset)
                |> Maybe.map
                    (\balance ->
                        MaxButton.disabled
                            { token = asset
                            , balance = balance
                            }
                    )
                |> Maybe.withDefault none
            ]
        , Textbox.disabled model
            { token = asset
            , text = transaction.assetIn
            , description = "lend asset textbox"
            }
        ]


claimsOutSection :
    { model | images : Images }
    -> Pool
    -> { transaction | claimsOut : ClaimsOut }
    -> Element Never
claimsOutSection model pool { claimsOut } =
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
                Mode.Recommended

            _ ->
                Mode.Advanced
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
            |> Maybe.map Slider.disabled
            |> Maybe.withDefault none
        , row
            [ width fill
            , height shrink
            , spacing 16
            ]
            [ Info.emptyAPR
            , Info.emptyCDP
            ]
        , case claimsOut of
            Default ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ pool.pair
                        |> Pair.toAsset
                        |> Just
                        |> bondOutSection model
                    , pool.pair
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
                        |> advancedBondOutSection model
                            (pool.pair |> Pair.toAsset)
                    , Nothing
                        |> advancedInsuranceOutSection model
                            (pool.pair |> Pair.toCollateral)
                    ]

            Bond { bondOut } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ bondOut
                        |> Just
                        |> advancedBondOutSection model
                            (pool.pair |> Pair.toAsset)
                    , Nothing
                        |> advancedInsuranceOutSection model
                            (pool.pair |> Pair.toCollateral)
                    ]

            Insurance { insuranceOut } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ Nothing
                        |> advancedBondOutSection model
                            (pool.pair |> Pair.toAsset)
                    , insuranceOut
                        |> Just
                        |> advancedInsuranceOutSection model
                            (pool.pair |> Pair.toCollateral)
                    ]
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
                    Output.empty model
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
                    Output.empty model
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


advancedBondOutSection :
    { model | images : Images }
    -> Token
    -> Maybe String
    -> Element Never
advancedBondOutSection model asset input =
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


advancedInsuranceOutSection :
    { model | images : Images }
    -> Token
    -> Maybe String
    -> Element Never
advancedInsuranceOutSection model collateral input =
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
