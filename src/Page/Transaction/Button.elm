module Page.Transaction.Button exposing
    ( approveAsset
    , approveCollateral
    , connect
    , doesNotExist
    , error
    , exist
    , loading
    , matured
    , selectMaturity
    , selectTokens
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
import Http
import Utility.Color as Color


connect : msg -> Element msg
connect msg =
    Input.button
        [ Region.description "connect wallet"
        , width <| px 343
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
        , width <| px 343
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
        , width <| px 343
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


selectTokens : Element Never
selectTokens =
    el
        [ Region.description "select token"
        , width <| px 343
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
            (text "Select Tokens First")
        )


selectMaturity : Element Never
selectMaturity =
    el
        [ Region.description "select maturity"
        , width <| px 343
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
            (text "Select Maturity First")
        )


loading : Element Never
loading =
    el
        [ Region.description "loading"
        , width <| px 343
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
            (text "Loading")
        )


matured : Element Never
matured =
    el
        [ Region.description "matured"
        , width <| px 343
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
            (text "Matured")
        )


doesNotExist : Element Never
doesNotExist =
    el
        [ Region.description "does not exist"
        , width <| px 343
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
            (text "Pool Does Not Exist")
        )


exist : Element Never
exist =
    el
        [ Region.description "exist"
        , width <| px 343
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
            (text "Pool Already Exist")
        )


error : Http.Error -> Element Never
error httpError =
    el
        [ width <| px 343
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
            (text "error")
        )
