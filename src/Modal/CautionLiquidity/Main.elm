module Modal.CautionLiquidity.Main exposing (Effect(..), Modal, Msg, init, update, view)

import Blockchain.User.WriteLiquidity exposing (WriteLiquidity(..))
import Data.Backdrop exposing (Backdrop)
import Data.Images exposing (Images)
import Data.Theme exposing (Theme)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
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
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.ThemeColor as ThemeColor


type Modal
    = Modal
        { txn : WriteLiquidity
        }


type Msg
    = ConfirmAddLiqClick WriteLiquidity
    | Exit


type Effect
    = AddLiquidity WriteLiquidity


init : WriteLiquidity -> Modal
init writeLiq =
    { txn = writeLiq
    }
        |> Modal


update : Msg -> Modal -> ( Maybe Modal, Maybe Effect )
update msg (Modal modal) =
    case msg of
        ConfirmAddLiqClick writeLiq ->
            ( modal |> Modal |> Just
            , AddLiquidity writeLiq |> Just
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
                [ header model
                , body model modal
                ]
        }


header :
    { model | images : Images, backdrop : Backdrop, theme : Theme }
    -> Element Msg
header ({ theme } as model) =
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
            ("Note"
                |> text
            )
        , IconButton.exit model Exit
        ]


body :
    { model | images : Images, backdrop : Backdrop, theme : Theme }
    -> Modal
    -> Element Msg
body { images, theme } (Modal { txn }) =
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
            , paragraph
                [ width shrink
                , Font.size 14
                , Font.alignLeft
                , paddingXY 0 8
                , spacing 6
                ]
                [ text "You can withdraw your liquidity only after pool maturity."
                , text " Do note as an LP you are subject to divergence loss if there is change in price of asset or collateral."
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
            { onPress = Just (ConfirmAddLiqClick txn)
            , label = text "Confirm"
            }
        ]
