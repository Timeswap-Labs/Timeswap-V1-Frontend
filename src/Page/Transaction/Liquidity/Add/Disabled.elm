module Page.Transaction.Liquidity.Add.Disabled exposing
    ( Transaction(..)
    , init
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Data.Images exposing (Images)
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Theme exposing (Theme)
import Data.Token exposing (Token)
import Element
    exposing
        ( Element
        , alpha
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
        , paddingXY
        , paragraph
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
import Page.Transaction.Textbox as Textbox
import Utility.Color as Color
import Utility.Image as Image
import Utility.ThemeColor as ThemeColor


type Transaction
    = Asset String
    | Collateral String


init : Transaction
init =
    Asset ""


view :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    -> Transaction
    ->
        { first : Element Never
        , second : Element Never
        , third : Element Never
        }
view model blockchain pool transaction =
    { first =
        assetInSection model
            blockchain
            pool
            transaction
    , second = duesInSection model blockchain pool transaction
    , third = warningSection model pool
    }


assetInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    -> Transaction
    -> Element Never
assetInSection model blockchain pool transaction =
    column
        [ Region.description "lend asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , alpha 0.2
        , model.theme |> ThemeColor.sectionBackground |> Background.color
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
                , model.theme |> ThemeColor.actionElemLabel |> Font.color
                ]
                (text "Add Asset")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance (pool.pair |> Pair.toAsset))
                |> Maybe.map
                    (\balance ->
                        MaxButton.disabled
                            { token = pool.pair |> Pair.toAsset
                            , balance = balance
                            , theme = model.theme
                            }
                    )
                |> Maybe.withDefault none
            ]
        , (case transaction of
            Asset assetIn ->
                assetIn

            _ ->
                ""
          )
            |> (\assetIn ->
                    Textbox.disabled model
                        { token = pool.pair |> Pair.toAsset
                        , text = assetIn
                        , description = "asset in textbox"
                        }
               )
        , collateralInSection model
            blockchain
            (pool.pair |> Pair.toCollateral)
            transaction
        ]


collateralInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Token
    -> Transaction
    -> Element Never
collateralInSection model blockchain collateral transaction =
    column
        [ width fill
        , height shrink
        , spacing 10
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
                , model.theme |> ThemeColor.actionElemLabel |> Font.color
                ]
                (text "Add Collateral")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance collateral)
                |> Maybe.map
                    (\balance ->
                        MaxButton.disabled
                            { token = collateral
                            , balance = balance
                            , theme = model.theme
                            }
                    )
                |> Maybe.withDefault none
            ]
        , (case transaction of
            Collateral collateralOut ->
                collateralOut

            _ ->
                ""
          )
            |> (\collateralOut ->
                    Textbox.disabled model
                        { token = collateral
                        , text = collateralOut
                        , description = "collateral output"
                        }
               )
        ]


duesInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    -> Transaction
    -> Element Never
duesInSection model blockchain pool transaction =
    column
        [ spacing 16
        , alpha 0.2
        ]
        [ column
            [ Region.description "dues"
            , width <| px 343
            , height shrink
            , padding 16
            , spacing 12
            , model.theme |> ThemeColor.sectionBackground |> Background.color
            , Border.rounded 8
            ]
            [ row
                [ width fill
                , height shrink
                , spacing 16
                ]
                [ Info.emptyAPR model.theme
                , Info.emptyCDP model.theme
                ]
            ]
        , column
            [ width fill
            , height shrink
            , spacing 12
            , padding 16
            , model.theme |> ThemeColor.sectionBackground |> Background.color
            , Border.rounded 8
            ]
            [ debtInSection model
                (pool.pair |> Pair.toAsset)
                transaction
            ]
        ]


debtInSection :
    { model | images : Images, theme : Theme }
    -> Token
    -> Transaction
    -> Element Never
debtInSection model asset transaction =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , model.theme |> ThemeColor.textLight |> Font.color
            ]
            (text "Debt to Repay")
        , el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , model.theme |> ThemeColor.textLight |> Font.color
            ]
            (text "Pool Share")
        ]


warningSection :
    { model | images : Images, theme : Theme }
    -> Pool
    -> Element Never
warningSection { images, theme } pool =
    column
        [ width fill
        , height shrink
        , centerX
        , padding 16
        , spacing 12
        , Font.size 14
        , alpha 0.2
        , Border.rounded 8
        , theme |> ThemeColor.sectionBackground |> Background.color
        ]
        [ images
            |> Image.warning
                [ width <| px 24, height <| px 24, Font.center, centerX ]
        , paragraph
            [ Font.color Color.warning400
            , Font.center
            ]
            [ text "The above Debt must be repaid before maturity of the pool, or else the collateral locked will be forfeited. You can view the debt position under the Borrow tab." ]
        ]
