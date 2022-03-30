module Modal.Caution.Main exposing (Effect(..), Modal, Msg, init, update, view)

import Blockchain.User.WriteLend exposing (WriteLend)
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
        { txn : WriteLend
        }


type Msg
    = LendClick WriteLend
    | Exit


type Effect
    = Lend WriteLend


init : WriteLend -> Modal
init writeLend =
    { txn = writeLend
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
            (text "Caution : Low CDP")
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
                , Font.alignLeft
                ]
                [ text "Your lending transaction will be under-collateralized (CDP < 100%) and there is a high probability of principal loss. Please confirm you still want to lend."
                ]
            , paragraph
                [ width shrink
                , Font.size 14
                , Font.alignLeft
                , paddingXY 0 8
                ]
                [ text "Alternatively, you can try to adjust your CDP or your transaction size to make it over-collateralized. If not, be patient and wait for pool to be over-collateralized."
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
            , label = text "Confirm Lend"
            }
        ]
