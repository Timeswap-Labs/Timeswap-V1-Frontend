module Modal.Caution.Main exposing (Effect(..), Modal, Msg, init, update, view)

import Blockchain.User.WriteLend exposing (WriteLend(..))
import Data.Backdrop exposing (Backdrop)
import Data.CDP exposing (CDP)
import Data.Images exposing (Images)
import Data.Pair as Pair
import Data.Theme exposing (Theme)
import Data.Token as Token
import Element
    exposing
        ( Element
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
import Element.Input as Input
import Modal.Caution.Tooltip as Tooltip exposing (Tooltip)
import Modal.Outside as Outside
import Page.PoolInfo exposing (PoolInfo)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.ThemeColor as ThemeColor
import Utility.Truncate as Truncate


type Modal
    = Modal
        { txn : WriteLend
        , apr : Float
        , cdp : CDP
        , poolInfo : PoolInfo
        , tooltip : Maybe Tooltip
        }


type Msg
    = LendClick WriteLend
    | OnMouseEnter Tooltip
    | OnMouseLeave
    | Exit


type Effect
    = Lend WriteLend


init : WriteLend -> Float -> CDP -> PoolInfo -> Modal
init writeLend apr cdp poolInfo =
    { txn = writeLend
    , apr = apr
    , cdp = cdp
    , poolInfo = poolInfo
    , tooltip = Nothing
    }
        |> Modal


update : Msg -> Modal -> ( Maybe Modal, Maybe Effect )
update msg (Modal modal) =
    case msg of
        LendClick writeLend ->
            ( modal |> Modal |> Just
            , Lend writeLend |> Just
            )

        OnMouseEnter tooltip ->
            ( { modal | tooltip = Just tooltip }
                |> Modal
                |> Just
            , Nothing
            )

        OnMouseLeave ->
            ( { modal | tooltip = Nothing }
                |> Modal
                |> Just
            , Nothing
            )

        Exit ->
            ( Nothing, Nothing )


view :
    { model | images : Images, backdrop : Backdrop, theme : Theme }
    -> Modal
    -> Element Msg
view ({ backdrop, theme } as model) modal =
    Outside.view model
        { onClick = Exit
        , modal =
            column
                ([ width <| px 375
                 , height shrink
                 , centerX
                 , centerY
                 , Border.rounded 8
                 , theme |> ThemeColor.border |> Border.color
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop theme
                )
                [ header model modal
                , body model modal
                ]
        }


header :
    { model | images : Images, backdrop : Backdrop, theme : Theme }
    -> Modal
    -> Element Msg
header ({ theme } as model) (Modal { cdp }) =
    row
        [ width fill
        , height shrink
        , spacing 16
        , padding 24
        , Border.widthEach
            { top = 0
            , right = 0
            , bottom = 1
            , left = 0
            }
        , theme |> ThemeColor.textboxBorder |> Border.color
        ]
        [ el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 18
            , Font.bold
            , paddingXY 0 3
            , theme |> ThemeColor.text |> Font.color
            ]
            ((case cdp.percent of
                Just cdpPerc ->
                    if cdpPerc * 100 > 100 then
                        "Note"

                    else
                        "Caution : Low CDP"

                _ ->
                    "Note"
             )
                |> text
            )
        , IconButton.exit model Exit
        ]


body :
    { model | images : Images, backdrop : Backdrop, theme : Theme }
    -> Modal
    -> Element Msg
body { images, theme } (Modal { txn, cdp, tooltip }) =
    column
        [ width fill
        , height shrink
        , padding 24
        , spacing 24
        , centerX
        ]
        [ column
            [ width fill
            , height shrink
            , padding 16
            , spacing 12
            , centerX
            , Font.size 14
            , theme |> ThemeColor.text |> Font.color
            , theme |> ThemeColor.sectionBackground |> Background.color
            , Border.rounded 8
            ]
            [ images
                |> Image.warningCircle
                    [ width <| px 30
                    , height <| px 30
                    , centerX
                    , centerY
                    ]
            , case cdp.percent of
                Just percent ->
                    if (percent * 100) < 100 then
                        paragraph
                            [ width shrink
                            , Font.alignLeft
                            , Font.color Color.negative500
                            ]
                            [ text "This is an under collateralized lending transaction (CDP < 100%), and there is a high risk of Principal loss."
                            ]

                    else
                        none

                _ ->
                    none
            , paragraph
                [ width shrink
                , Font.size 14
                , Font.alignLeft
                , paddingXY 0 8
                , spacing 6
                ]
                [ text "Your expected principal along with interest is "
                , (case txn of
                    GivenPercent givenPercent ->
                        ( givenPercent.minBond, givenPercent.pool )

                    GivenBond givenBond ->
                        ( givenBond.bondOut, givenBond.pool )

                    GivenInsurance givenIns ->
                        ( givenIns.minBond, givenIns.pool )
                  )
                    |> (\( bond, pool ) ->
                            row [ spacing 4 ]
                                [ el [ Font.bold ] (pool.pair |> Pair.toAsset |> Token.toSymbol |> text)
                                , Truncate.viewAmount
                                    { onMouseEnter = OnMouseEnter
                                    , onMouseLeave = OnMouseLeave
                                    , tooltip = Tooltip.AssetReturn
                                    , opened = tooltip
                                    , token = pool.pair |> Pair.toAsset
                                    , amount = bond
                                    , theme = theme
                                    , customStyles =
                                        [ Font.size 14
                                        , Font.bold
                                        , paddingEach
                                            { top = 4
                                            , right = 0
                                            , bottom = 1
                                            , left = 0
                                            }
                                        ]
                                    }
                                ]
                       )
                , text " after maturity"
                , (case cdp.percent of
                    Just cdpPerc ->
                        (if cdpPerc > 1 then
                            [ ", with the assumption that the collateral price will not fall more than "
                            , ((cdpPerc - 1) / cdpPerc) * 10000 |> round |> toFloat |> (\float -> float / 100) |> String.fromFloat
                            , "% from the current price, in which case the realized APR can be lower."
                            ]

                         else
                            [ ". However, due to the transaction being undercollateralized, your realized APR will depend on the amount of defaults & the price of collateral at maturity."
                            ]
                        )
                            |> String.concat

                    _ ->
                        ""
                  )
                    |> text
                ]
            ]
        , Input.button
            [ width fill
            , height <| px 44
            , centerX
            , Font.center
            , padding 10
            , Font.size 16
            , Font.color Color.light100
            , theme |> ThemeColor.primaryBtn |> Background.color
            , Border.rounded 4
            ]
            { onPress = Just (LendClick txn)
            , label = text "Confirm"
            }
        ]
