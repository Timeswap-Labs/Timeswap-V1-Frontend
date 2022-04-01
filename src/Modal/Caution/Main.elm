module Modal.Caution.Main exposing (Effect(..), Modal, Msg, init, update, view)

import BigInt
import Blockchain.User.WriteLend exposing (WriteLend(..))
import Data.Backdrop exposing (Backdrop)
import Data.CDP exposing (CDP)
import Data.Images exposing (Images)
import Data.Pair as Pair
import Data.Theme exposing (Theme)
import Data.Uint as Uint
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
import Modal.Outside as Outside
import Page.PoolInfo exposing (PoolInfo)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.ThemeColor as ThemeColor


type Modal
    = Modal
        { txn : WriteLend
        , apr : Float
        , cdp : CDP
        , poolInfo : PoolInfo
        }


type Msg
    = LendClick WriteLend
    | Exit


type Effect
    = Lend WriteLend


init : WriteLend -> Float -> CDP -> PoolInfo -> Modal
init writeLend apr cdp poolInfo =
    { txn = writeLend
    , apr = apr
    , cdp = cdp
    , poolInfo = poolInfo
    }
        |> Modal


update : Msg -> Modal -> ( Maybe Modal, Maybe Effect )
update msg modal =
    case msg of
        LendClick writeLend ->
            ( modal |> Just
            , Lend writeLend |> Just
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
body { images, theme } (Modal { txn, apr, cdp, poolInfo }) =
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
                            [ text "You are doing an under-collateralized lending (CDP < 100%). You are under-insured for default risk and can lose on Principal."
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
                ]
                [ text "Expected return is $"
                , (case txn of
                    GivenPercent givenPercent ->
                        ( givenPercent.minBond, givenPercent.pool )

                    GivenBond givenBond ->
                        ( givenBond.bondOut, givenBond.pool )

                    GivenInsurance givenIns ->
                        ( givenIns.minBond, givenIns.pool )
                  )
                    |> (\( bond, pool ) ->
                            case poolInfo.assetSpot of
                                Just assetSpot ->
                                    let
                                        maybeAssetSpotBigInt =
                                            (assetSpot * 10000) |> round |> String.fromInt |> BigInt.fromIntString

                                        maybeBondBigInt =
                                            bond |> Uint.toString |> BigInt.fromIntString
                                    in
                                    case ( maybeBondBigInt, maybeAssetSpotBigInt ) of
                                        ( Just bondBigInt, Just assetSpotBigInt ) ->
                                            BigInt.mul bondBigInt assetSpotBigInt
                                                |> (\bondInDollars -> BigInt.div bondInDollars (10000 |> BigInt.fromInt))
                                                |> BigInt.toString
                                                |> Uint.fromString
                                                |> Maybe.map (\uint -> Uint.toAmountTruncated (pool.pair |> Pair.toAsset) uint)
                                                |> Maybe.withDefault "(err)"

                                        _ ->
                                            "(err)"

                                _ ->
                                    "(err)"
                       )
                    |> text
                , text " but with the expectation of collateral price "
                , (case cdp.percent of
                    Just cdpPerc ->
                        (if (cdpPerc * 100) > 100 then
                            [ "not falling below "
                            , (cdpPerc * 100 - 100) * 100 |> round |> toFloat |> (\int -> int / 100) |> String.fromFloat
                            , "% "
                            ]

                         else
                            [ "going up by more than "
                            , (100 - cdpPerc * 100) * 100 |> round |> toFloat |> (\int -> int / 100) |> String.fromFloat
                            , "% "
                            ]
                        )
                            |> String.concat

                    _ ->
                        ""
                  )
                    |> text
                , text "from the current price at maturity. "
                , (case cdp.percent of
                    Just cdpPerc ->
                        if (cdpPerc * 100) > 100 then
                            [ "If the collateral price falls below "
                            , (cdpPerc * 100 - 100) * 100 |> round |> toFloat |> (\int -> int / 100) |> String.fromFloat
                            , "%"
                            ]
                                |> String.concat

                        else
                            [ "If the collateral price does not go up more than "
                            , (100 - cdpPerc * 100) * 100 |> round |> toFloat |> (\int -> int / 100) |> String.fromFloat
                            , "%"
                            ]
                                |> String.concat

                    _ ->
                        ""
                  )
                    |> text
                , text ", there is a high probability of default risk and not realizing an APR of "
                , text ([ String.fromFloat (apr * 100), "%" ] |> String.join "")
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
