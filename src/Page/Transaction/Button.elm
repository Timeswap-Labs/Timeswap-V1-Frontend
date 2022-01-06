module Page.Transaction.Button exposing
    ( approve
    , approveAsset
    , approveCollateral
    , checkingAllowance
    , checkingBalance
    , connect
    , disabled
    , disabledApprove
    , doesNotExist
    , error
    , exist
    , loading
    , matured
    , notEnoughBalance
    , pendingApprove
    , selectMaturity
    , selectTokens
    , view
    )

import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Theme exposing (Theme)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , fill
        , height
        , paddingXY
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
import Utility.ThemeColor as ThemeColor


view :
    { onPress : msg
    , text : String
    }
    -> Element msg
view params =
    Input.button
        [ Region.description params.text
        , width fill
        , height <| px 44
        , Background.color Color.primary500
        , Border.rounded 4
        ]
        { onPress = Just params.onPress
        , label =
            el
                [ centerX
                , centerY
                , Font.size 16
                , paddingXY 0 4
                , Font.color Color.light100
                ]
                (text params.text)
        }


disabled : String -> Element Never
disabled string =
    el
        [ Region.description string
        , width fill
        , height <| px 44
        , Background.color Color.primary100
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , Font.color Color.transparent100
            ]
            (text string)
        )


approve :
    { onPress : msg
    , erc20 : ERC20
    }
    -> Element msg
approve params =
    Input.button
        [ [ "approve"
          , params.erc20
                |> ERC20.toSymbol
          ]
            |> String.join " "
            |> Region.description
        , width fill
        , height <| px 44
        , Background.color Color.primary500
        , Border.rounded 4
        ]
        { onPress = Just params.onPress
        , label =
            el
                [ centerX
                , centerY
                , Font.size 16
                , paddingXY 0 4
                , Font.color Color.light100
                ]
                ([ "Approve"
                 , params.erc20
                    |> ERC20.toSymbol
                    |> String.left 5
                 ]
                    |> String.join " "
                    |> text
                )
        }


checkingAllowance : Theme -> Element Never
checkingAllowance theme =
    el
        [ Region.description "checking allowance"
        , width fill
        , height <| px 44
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Font.size 16
            , Font.bold
            , paddingXY 0 4
            , theme |> ThemeColor.textDisabled |> Font.color
            ]
            (text "Checking Allowance")
        )


checkingBalance : Theme -> Element Never
checkingBalance theme =
    el
        [ Region.description "checking balance"
        , width fill
        , height <| px 44
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Font.size 16
            , Font.bold
            , paddingXY 0 4
            , theme |> ThemeColor.textDisabled |> Font.color
            ]
            (text "Checking Balance")
        )


disabledApprove : ERC20 -> Element msg
disabledApprove erc20 =
    el
        [ [ "approve"
          , erc20
                |> ERC20.toSymbol
          ]
            |> String.join " "
            |> Region.description
        , width fill
        , height <| px 44
        , Background.color Color.primary100
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , Font.color Color.transparent100
            ]
            ([ "Approve"
             , erc20
                |> ERC20.toSymbol
                |> String.left 5
             ]
                |> String.join " "
                |> text
            )
        )


pendingApprove : ERC20 -> Element msg
pendingApprove erc20 =
    el
        [ [ "approve"
          , erc20
                |> ERC20.toSymbol
          ]
            |> String.join " "
            |> Region.description
        , width fill
        , height <| px 44
        , Background.color Color.primary100
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , Font.color Color.transparent100
            ]
            ([ "approving"
             , erc20
                |> ERC20.toSymbol
                |> String.left 5
             ]
                |> String.join " "
                |> text
            )
        )


connect : Theme -> msg -> Element msg
connect theme msg =
    Input.button
        [ Region.description "connect wallet"
        , width fill
        , height <| px 44
        , theme |> ThemeColor.primaryBtn |> Background.color
        , Border.rounded 4
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                , paddingXY 0 4
                , Font.size 16
                , Font.bold
                , Font.color Color.light100
                ]
                (text "Connect Wallet")
        }


approveAsset : msg -> Element msg
approveAsset msg =
    Input.button
        [ Region.description "approve asset"
        , width fill
        , height <| px 44
        , Background.color Color.primary500
        , Border.rounded 4
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                , Font.size 16
                , Font.bold
                , paddingXY 0 4
                , Font.color Color.light100
                ]
                (text "Approve Asset")
        }


approveCollateral : msg -> Element msg
approveCollateral msg =
    Input.button
        [ Region.description "approve collateral"
        , width fill
        , height <| px 44
        , Background.color Color.primary500
        , Border.rounded 4
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                , paddingXY 0 4
                , Font.size 16
                , Font.bold
                , Font.color Color.light100
                ]
                (text "Approve Collateral")
        }


selectTokens : Element Never
selectTokens =
    el
        [ Region.description "select token"
        , width fill
        , height <| px 44
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , Font.color Color.transparent100
            ]
            (text "Select Tokens First")
        )


selectMaturity : Element Never
selectMaturity =
    el
        [ Region.description "select maturity"
        , width fill
        , height <| px 44
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , Font.color Color.transparent100
            ]
            (text "Select Maturity First")
        )


loading : Element Never
loading =
    el
        [ Region.description "loading"
        , width fill
        , height <| px 44
        , Background.color Color.primary100
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , Font.color Color.transparent100
            ]
            (text "Loading")
        )


matured : Element Never
matured =
    el
        [ Region.description "matured"
        , width fill
        , height <| px 44
        , Background.color Color.primary100
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , Font.bold
            , Font.color Color.transparent100
            ]
            (text "Matured")
        )


doesNotExist : Element Never
doesNotExist =
    el
        [ Region.description "does not exist"
        , width fill
        , height <| px 44
        , Background.color Color.primary100
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , Font.bold
            , Font.color Color.transparent100
            ]
            (text "Pool Does Not Exist")
        )


exist : Element Never
exist =
    el
        [ Region.description "exist"
        , width fill
        , height <| px 44
        , Background.color Color.primary100
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , Font.bold
            , Font.color Color.transparent100
            ]
            (text "Pool Already Exists")
        )


error : Http.Error -> Element Never
error httpError =
    el
        [ width fill
        , height <| px 44
        , Background.color Color.negative500
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , Font.color Color.light100
            ]
            (text "error")
        )


notEnoughBalance : Element msg
notEnoughBalance =
    el
        [ width fill
        , height <| px 44
        , Background.color Color.negative500
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , Font.bold
            , Font.color Color.light100
            ]
            (text "Not Enough Balance")
        )
