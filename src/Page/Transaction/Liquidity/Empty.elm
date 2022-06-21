module Page.Transaction.Liquidity.Empty exposing (view)

import Data.Images exposing (Images)
import Data.Pair as Pair
import Data.Theme exposing (Theme)
import Data.Token exposing (Token)
import Element
    exposing
        ( Element
        , alpha
        , centerX
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
import Page.Transaction.Output as Output
import Page.Transaction.Textbox as Textbox
import Utility.Color as Color
import Utility.Image as Image
import Utility.Maybe as Maybe
import Utility.ThemeColor as ThemeColor


view :
    { model | images : Images, theme : Theme }
    ->
        { asset : Maybe Token
        , collateral : Maybe Token
        }
    ->
        { first : Element Never
        , second : Element Never
        , third : Element Never
        }
view model { asset, collateral } =
    { first = assetInSection model asset collateral
    , second = duesOutSection model asset
    , third = warningSection model
    }


assetInSection :
    { model | images : Images, theme : Theme }
    -> Maybe Token
    -> Maybe Token
    -> Element Never
assetInSection model asset collateral =
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
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , model.theme |> ThemeColor.actionElemLabel |> Font.color
            ]
            (text "Add Asset")
        , asset
            |> Maybe.map
                (\assetToken ->
                    Textbox.disabled model
                        { token = assetToken
                        , text = ""
                        , description = "asset in textbox"
                        }
                )
            |> Maybe.withDefault
                (Textbox.empty "asset in textbox")
        , collateralOutSection model collateral
        ]


duesOutSection :
    { model | images : Images, theme : Theme }
    -> Maybe Token
    -> Element Never
duesOutSection model asset =
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
            [ debtOutSection model asset
            ]
        ]


debtOutSection :
    { model | images : Images, theme : Theme }
    -> Maybe Token
    -> Element Never
debtOutSection model asset =
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


collateralOutSection :
    { model | images : Images, theme : Theme }
    -> Maybe Token
    -> Element Never
collateralOutSection model collateral =
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
            , model.theme |> ThemeColor.actionElemLabel |> Font.color
            ]
            (text "Collateral to Lock")
        , collateral
            |> Maybe.map
                (\token ->
                    Textbox.disabled model
                        { token = token
                        , text = ""
                        , description = "collateral output"
                        }
                )
            |> Maybe.withDefault
                (Textbox.empty "collateral output")
        ]


warningSection :
    { model | images : Images, theme : Theme }
    -> Element Never
warningSection { images, theme } =
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
