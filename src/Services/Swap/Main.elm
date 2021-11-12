module Services.Swap.Main exposing (..)

import Data.Backdrop exposing (Backdrop)
import Data.Device as Device exposing (Device)
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenImages exposing (TokenImages)
import Data.Tokens as Tokens exposing (Tokens)
import Dropdown
import Element
    exposing
        ( Element
        , alignBottom
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , inFront
        , link
        , mouseDown
        , mouseOver
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
import Json.Encode as Encode exposing (Value)
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Router as Router
import Utility.TokenImage as TokenImage


type GameToken
    = Token


type alias Item =
    String


type alias DropdownModel =
    { dropdownState : Dropdown.State Item
    , selectedOption : Maybe Item
    }


init : () -> ( DropdownModel, Cmd Msg )
init _ =
    ( { dropdownState = Dropdown.init "dropdown"
      , selectedOption = Nothing
      }
    , Cmd.none
    )


options : List Item
options =
    [ "Option 1", "Option 2", "Option 3" ]


type Msg
    = OptionPicked (Maybe Item)
    | DropdownMsg (Dropdown.Msg Item)


update : Msg -> DropdownModel -> ( DropdownModel, Cmd Msg )
update msg model =
    case msg of
        OptionPicked option ->
            ( { model | selectedOption = option }, Cmd.none )

        DropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update dropdownConfig subMsg model model.dropdownState
            in
            ( { model | dropdownState = state }, cmd )


view :
    { model
        | device : Device
        , backdrop : Backdrop
        , tokens : Tokens
        , images : Images
        , tokenImages : TokenImages
        , dd : DropdownModel
        , user : Remote userError user
    }
    -> Element Msg
view ({ device, backdrop, images, user } as model) =
    column
        ([ padding 40
         , spacing 24
         , centerX
         , centerY
         , Exit.button images |> inFront
         ]
            ++ Glass.lightPrimaryModal backdrop 0
            ++ (if Device.isPhone device then
                    [ width fill
                    , height shrink
                    , alignBottom
                    ]

                else
                    [ width <| px 533
                    , height shrink
                    ]
               )
        )
        ([ title
         , content model
         ]
            ++ (case user of
                    Success _ ->
                        []

                    _ ->
                        [ swapButton model ]
               )
        )


title : Element msg
title =
    el
        [ width shrink
        , height shrink
        , paddingXY 0 4
        , centerX
        , Font.family [ Font.typeface "Supreme" ]
        , Font.bold
        , Font.size 24
        , Font.color Color.light100
        , Font.center
        ]
        (text "Swap Your Token")


content : { model | tokens : Tokens, images : Images, tokenImages : TokenImages, dd : DropdownModel, user : Remote userError user } -> Element Msg
content model =
    column
        [ width fill
        , height shrink
        , spacing 14
        ]
        [ row []
            [ Dropdown.view dropdownConfig model.dd model.dd.dropdownState
                |> el []
            ]
        , row []
            [ Input.text [ width fill ]
                { onChange = ip
                , text = "test"
                , placeholder =
                    Input.placeholder
                        [ Font.color Color.transparent100 ]
                        (text "0.0")
                        |> Just
                , label = Input.labelHidden "Input Amount"
                }
            ]
        ]


swapButton : { model | device : Device, images : Images } -> Element msg
swapButton { device, images } =
    link
        ([ width fill
         , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
         , centerY
         , Background.color Color.primary500
         , Border.rounded 4
         , Font.size 16
         , Font.color Color.light100
         , mouseDown [ Background.color Color.primary400 ]
         , mouseOver [ Background.color Color.primary300 ]
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35 ]

                else
                    [ height <| px 44 ]
               )
        )
        { url = Router.toConnect
        , label =
            row
                [ width shrink
                , height fill
                , spacing 6
                , centerX
                ]
                (Image.wallet images
                    [ width <| px 24
                    , centerY
                    ]
                    :: (if Device.isPhone device then
                            []

                        else
                            [ el [ centerY, Font.regular ]
                                (if Device.isTablet device then
                                    text "Swap"

                                 else
                                    text "Swap tokens"
                                )
                            ]
                       )
                )
        }


dropdownConfig : Dropdown.Config Item Msg DropdownModel
dropdownConfig =
    let
        itemToPrompt item =
            text item

        itemToElement selected highlighted item =
            text item
    in
    Dropdown.basic
        { itemsFromModel = always options
        , selectionFromModel = .selectedOption
        , dropdownMsg = DropdownMsg
        , onSelectMsg = OptionPicked
        , itemToPrompt = itemToPrompt
        , itemToElement = itemToElement
        }
