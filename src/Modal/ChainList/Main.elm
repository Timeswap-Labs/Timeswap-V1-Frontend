port module Modal.ChainList.Main exposing
    ( Effect(..)
    , Msg
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Data.Backdrop exposing (Backdrop)
import Data.Chain as Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Images exposing (Images)
import Data.Support exposing (Support(..))
import Data.Theme exposing (Theme)
import Element
    exposing
        ( Element
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , mouseOver
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
import Element.Input as Input
import Json.Encode exposing (Value)
import Modal.Outside as Outside
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.ThemeColor as ThemeColor


type Msg
    = ClickChain Chain
    | Exit


type Effect
    = ChangeChain Chain


update :
    Support User.NotSupported Blockchain
    -> Msg
    -> ( Maybe (), Cmd Msg, Maybe Effect )
update support msg =
    case msg of
        ClickChain chain ->
            case support of
                Supported blockchain ->
                    case
                        ( blockchain |> Blockchain.toUser
                        , (blockchain |> Blockchain.toChain) == chain
                        )
                    of
                        ( _, True ) ->
                            ( Just ()
                            , Cmd.none
                            , Nothing
                            )

                        ( Just _, False ) ->
                            ( Nothing
                            , chain
                                |> Chain.encode
                                |> changeChain
                            , Nothing
                            )

                        ( Nothing, False ) ->
                            ( Nothing
                            , Cmd.none
                            , ChangeChain chain
                                |> Just
                            )

                _ ->
                    ( Nothing
                    , chain
                        |> Chain.encode
                        |> changeChain
                    , Nothing
                    )

        Exit ->
            ( Nothing
            , Cmd.none
            , Nothing
            )


port changeChain : Value -> Cmd msg


view :
    { model
        | backdrop : Backdrop
        , images : Images
        , chains : Chains
        , blockchain : Support User.NotSupported Blockchain
        , theme : Theme
    }
    -> Element Msg
view ({ backdrop, theme } as model) =
    Outside.view model
        { onClick = Exit
        , modal =
            column
                ([ width <| px 375
                 , height shrink
                 , padding 24
                 , centerX
                 , centerY
                 , spacing 16
                 , Border.rounded 8
                 , theme |> ThemeColor.border |> Border.color
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop theme
                )
                [ row
                    [ width fill
                    , height shrink
                    , spacing 16
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
                        (text "Change Chain")
                    , IconButton.exit model Exit
                    ]
                , column [ width fill, spacing 12 ]
                    (model.chains
                        |> Chains.toList
                        |> List.map (\chain -> chain |> chainRow model)
                    )
                ]
        }


chainRow :
    { model
        | images : Images
        , chains : Chains
        , blockchain : Support User.NotSupported Blockchain
        , theme : Theme
    }
    -> Chain
    -> Element Msg
chainRow model chain =
    case model.blockchain of
        Supported bc ->
            if (bc |> Blockchain.toChain |> Chain.toChainId) == (chain |> Chain.toChainId) then
                row
                    [ width fill
                    , height <| px 54
                    , paddingXY 18 0
                    , spacing 8
                    , Font.size 16
                    , model.theme |> ThemeColor.text |> Font.color
                    , model.theme |> ThemeColor.btnBackground |> Background.color
                    , Border.rounded 8
                    ]
                    [ model.images
                        |> Image.viewChain
                            [ width <| px 24
                            , height <| px 24
                            , centerY
                            ]
                            chain
                    , text (chain |> Chain.toString)
                    , el
                        [ width <| px 8
                        , height <| px 8
                        , alignRight
                        , centerY
                        , Background.color Color.positive500
                        , Border.rounded 4
                        ]
                        none
                    ]

            else
                Input.button
                    [ width fill ]
                    { onPress = chain |> ClickChain |> Just
                    , label =
                        row
                            [ width fill
                            , height <| px 54
                            , paddingXY 18 0
                            , spacing 8
                            , model.theme |> ThemeColor.btnBackground |> Background.color
                            , mouseOver [ model.theme |> ThemeColor.btnHoverBG |> Background.color ]
                            , Border.rounded 8
                            ]
                            [ model.images
                                |> Image.viewChain
                                    [ width <| px 24
                                    , height <| px 24
                                    , centerY
                                    ]
                                    chain
                            , el
                                [ width shrink
                                , height shrink
                                , centerY
                                , Font.size 16
                                , paddingXY 0 4
                                , model.theme |> ThemeColor.text |> Font.color
                                ]
                                (chain
                                    |> Chain.toString
                                    |> text
                                )
                            ]
                    }

        _ ->
            Input.button
                [ width fill ]
                { onPress = chain |> ClickChain |> Just
                , label =
                    row
                        [ width fill
                        , height <| px 54
                        , paddingXY 18 0
                        , spacing 8
                        , Background.color Color.primary100
                        , mouseOver [ Background.color Color.primary200 ]
                        , Border.rounded 8
                        ]
                        [ model.images
                            |> Image.viewChain
                                [ width <| px 24
                                , height <| px 24
                                , centerY
                                ]
                                chain
                        , el
                            [ width shrink
                            , height shrink
                            , centerY
                            , Font.size 16
                            , paddingXY 0 4
                            , Font.color Color.light100
                            ]
                            (chain
                                |> Chain.toString
                                |> text
                            )
                        ]
                }
