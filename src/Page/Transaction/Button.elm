module Page.Transaction.Button exposing
    ( approveAsset
    , approveCollateral
    , connect
    , disabled
    , error
    )

import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , height
        , px
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Utility.Color as Color


connect : msg -> Element msg
connect msg =
    Input.button
        [ Region.description "connect wallet"
        , width <| px 335
        , height <| px 44
        , Background.color Color.primary500
        , Border.rounded 8
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                , Font.size 14
                , Font.color Color.light100
                ]
                (text "Connect Wallet")
        }


approveAsset : msg -> Element msg
approveAsset msg =
    Input.button
        [ Region.description "approve asset"
        , width <| px 335
        , height <| px 44
        , Background.color Color.primary500
        , Border.rounded 8
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                , Font.size 14
                , Font.color Color.light100
                ]
                (text "Approve Asset")
        }


approveCollateral : msg -> Element msg
approveCollateral msg =
    Input.button
        [ Region.description "approve collateral"
        , width <| px 335
        , height <| px 44
        , Background.color Color.primary500
        , Border.rounded 8
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                , Font.size 14
                , Font.color Color.light100
                ]
                (text "Approve Collateral")
        }


disabled : String -> Element Never
disabled string =
    el
        [ width <| px 335
        , height <| px 44
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        (el
            [ centerX
            , centerY
            , Font.size 14
            , Font.color Color.transparent100
            ]
            (text string)
        )


error : String -> Element Never
error string =
    el
        [ width <| px 335
        , height <| px 44
        , Background.color Color.negative500
        , Border.rounded 8
        ]
        (el
            [ centerX
            , centerY
            , Font.size 14
            , Font.color Color.light100
            ]
            (text string)
        )
