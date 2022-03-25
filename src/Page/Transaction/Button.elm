module Page.Transaction.Button exposing
    ( approve
    , approveAsset
    , approveCollateral
    , checkingAllowance
    , checkingBalance
    , connect
    , customError
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
    , theme : Theme
    }
    -> Element msg
view params =
    Input.button
        [ Region.description params.text
        , width fill
        , height <| px 44
        , params.theme |> ThemeColor.primaryBtn |> Background.color
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


disabled : Theme -> String -> Element Never
disabled theme string =
    el
        [ Region.description string
        , width fill
        , height <| px 44
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , theme |> ThemeColor.textDisabled |> Font.color
            ]
            (text string)
        )


approve :
    { onPress : msg
    , erc20 : ERC20
    , theme : Theme
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
        , params.theme |> ThemeColor.primaryBtn |> Background.color
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
                    |> String.left 6
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


disabledApprove : Theme -> ERC20 -> Element msg
disabledApprove theme erc20 =
    el
        [ [ "approve"
          , erc20
                |> ERC20.toSymbol
          ]
            |> String.join " "
            |> Region.description
        , width fill
        , height <| px 44
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , theme |> ThemeColor.textDisabled |> Font.color
            ]
            ([ "Approve"
             , erc20
                |> ERC20.toSymbol
                |> String.left 6
             ]
                |> String.join " "
                |> text
            )
        )


pendingApprove : Theme -> ERC20 -> Element msg
pendingApprove theme erc20 =
    el
        [ [ "approve"
          , erc20
                |> ERC20.toSymbol
          ]
            |> String.join " "
            |> Region.description
        , width fill
        , height <| px 44
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , theme |> ThemeColor.textDisabled |> Font.color
            ]
            ([ "approving"
             , erc20
                |> ERC20.toSymbol
                |> String.left 6
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


approveAsset : msg -> Theme -> Element msg
approveAsset msg theme =
    Input.button
        [ Region.description "approve asset"
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
                , Font.size 16
                , Font.bold
                , paddingXY 0 4
                , Font.color Color.light100
                ]
                (text "Approve Asset")
        }


approveCollateral : msg -> Theme -> Element msg
approveCollateral msg theme =
    Input.button
        [ Region.description "approve collateral"
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
                (text "Approve Collateral")
        }


selectTokens : Theme -> Element Never
selectTokens theme =
    el
        [ Region.description "select token"
        , width fill
        , height <| px 44
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , theme |> ThemeColor.textDisabled |> Font.color
            ]
            (text "Select Tokens First")
        )


selectMaturity : Theme -> Element Never
selectMaturity theme =
    el
        [ Region.description "select maturity"
        , width fill
        , height <| px 44
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , theme |> ThemeColor.textDisabled |> Font.color
            ]
            (text "Select Maturity First")
        )


loading : Theme -> Element Never
loading theme =
    el
        [ Region.description "loading"
        , width fill
        , height <| px 44
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , theme |> ThemeColor.textDisabled |> Font.color
            ]
            (text "Loading")
        )


matured : Theme -> Element Never
matured theme =
    el
        [ Region.description "matured"
        , width fill
        , height <| px 44
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , Font.bold
            , theme |> ThemeColor.textDisabled |> Font.color
            ]
            (text "Matured")
        )


doesNotExist : Theme -> Element Never
doesNotExist theme =
    el
        [ Region.description "does not exist"
        , width fill
        , height <| px 44
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , Font.bold
            , theme |> ThemeColor.textDisabled |> Font.color
            ]
            (text "Pool Does Not Exist")
        )


exist : Theme -> Element Never
exist theme =
    el
        [ Region.description "exist"
        , width fill
        , height <| px 44
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , Font.bold
            , theme |> ThemeColor.textDisabled |> Font.color
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
            (text "Error")
        )


customError : String -> Element Never
customError errString =
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
            (text errString)
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
            (text "Insufficient Balance")
        )
