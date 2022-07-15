module Modal.PayTransaction.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Due exposing (Due)
import Blockchain.User.Dues as Dues
import Blockchain.User.Main as User exposing (User)
import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Blockchain.User.WritePay exposing (WritePay)
import Data.Address as Address
import Data.Backdrop exposing (Backdrop)
import Data.Chain as Chain
import Data.ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Theme as Theme exposing (Theme)
import Data.Token as Token
import Data.Uint as Uint exposing (Uint)
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
        , map
        , minimum
        , newTabLink
        , none
        , padding
        , paddingEach
        , paddingXY
        , px
        , row
        , scrollbarY
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
import Modal.PayTransaction.Error as Error exposing (Error)
import Modal.PayTransaction.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.Button as Button
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Textbox as Textbox
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)
import Url.Builder as Builder
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.Maybe as Maybe
import Utility.Result as Result
import Utility.ThemeColor as ThemeColor
import Utility.Truncate as Truncate


type Modal
    = Modal
        { pool : Pool
        , state : State
        , tooltip : Maybe Tooltip
        }


type State
    = Full (Set TokenId)
    | Custom (Dict TokenId String)


type Mode
    = RepayFull
    | RepayCustom


type Msg
    = SwitchMode Mode
    | InputAssetIn TokenId String
    | InputMax TokenId
    | ClickApprove
    | ClickPay
    | OnMouseEnter Tooltip
    | OnMouseLeave
    | Exit


type Effect
    = Approve ERC20
    | Pay WritePay


init :
    Pool
    -> Set TokenId
    -> ( Modal, Cmd Msg )
init pool set =
    ( { pool = pool
      , state = set |> Full
      , tooltip = Nothing
      }
        |> Modal
    , Cmd.none
    )


update :
    User
    -> Msg
    -> Modal
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update user msg (Modal modal) =
    case ( msg, modal.state ) of
        ( SwitchMode RepayFull, Custom dict ) ->
            dict
                |> Dict.keys
                |> Set.fromList TokenId.sorter
                |> (\set ->
                        ( { modal | state = set |> Full }
                            |> Modal
                            |> Just
                        , Cmd.none
                        , Nothing
                        )
                   )

        ( SwitchMode RepayCustom, Full set ) ->
            { modal
                | state =
                    user
                        |> User.getDues
                        |> Remote.map (Dues.getMultiple modal.pool set)
                        |> (Remote.map << Maybe.map << Dict.map)
                            (\_ { debt } ->
                                debt
                                    |> Uint.toAmount
                                        (modal.pool.pair |> Pair.toAsset)
                            )
                        |> (Remote.map << Maybe.withDefault) (Dict.empty TokenId.sorter)
                        |> Remote.withDefault (Dict.empty TokenId.sorter)
                        |> Custom
            }
                |> noCmdAndEffect

        ( InputAssetIn tokenId assetIn, Custom dict ) ->
            if assetIn |> Uint.isAmount (modal.pool.pair |> Pair.toAsset) then
                ( { modal
                    | state =
                        dict
                            |> Dict.insert tokenId assetIn
                            |> Custom
                  }
                    |> Modal
                    |> Just
                , Cmd.none
                , Nothing
                )

            else
                modal |> noCmdAndEffect

        ( InputMax tokenId, Custom dict ) ->
            case ( user |> User.getDues, user |> User.getBalance (modal.pool.pair |> Pair.toAsset) ) of
                ( Success dues, Just (Success balance) ) ->
                    dues
                        |> Dues.getSingle modal.pool tokenId
                        |> Maybe.map
                            (\due ->
                                (if Uint.hasEnough due.debt balance then
                                    due.debt

                                 else
                                    balance
                                )
                                    |> (\assetIn ->
                                            ( { modal
                                                | state =
                                                    dict
                                                        |> Dict.insert tokenId
                                                            (assetIn |> Uint.toAmount (modal.pool.pair |> Pair.toAsset))
                                                        |> Custom
                                              }
                                                |> Modal
                                                |> Just
                                            , Cmd.none
                                            , Nothing
                                            )
                                       )
                            )
                        |> Maybe.withDefault (modal |> noCmdAndEffect)

                _ ->
                    modal |> noCmdAndEffect

        ( ClickApprove, Full set ) ->
            (case
                ( getTotalFromFull user modal.pool set
                , modal.pool.pair
                    |> Pair.toAsset
                    |> Token.toERC20
                )
             of
                ( Ok total, Just erc20 ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (modal.pool.pair |> Pair.toAsset)
                                total
                        )
                            && (user
                                    |> User.hasEnoughAllowance
                                        erc20
                                        total
                                    |> not
                               )
                    then
                        ( modal
                            |> Modal
                            |> Just
                        , Cmd.none
                        , erc20
                            |> Approve
                            |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (modal |> noCmdAndEffect)

        ( ClickApprove, Custom dict ) ->
            (case
                ( getTotalFromCustom user modal.pool dict
                , modal.pool.pair
                    |> Pair.toAsset
                    |> Token.toERC20
                )
             of
                ( Ok total, Just erc20 ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (modal.pool.pair |> Pair.toAsset)
                                total
                        )
                            && (user
                                    |> User.hasEnoughAllowance
                                        erc20
                                        total
                                    |> not
                               )
                    then
                        ( modal
                            |> Modal
                            |> Just
                        , Cmd.none
                        , erc20
                            |> Approve
                            |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (modal |> noCmdAndEffect)

        ( ClickPay, Full set ) ->
            (case getTotalFromFull user modal.pool set of
                Ok total ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (modal.pool.pair |> Pair.toAsset)
                                total
                        )
                            && (modal.pool.pair
                                    |> Pair.toAsset
                                    |> Token.toERC20
                                    |> Maybe.map
                                        (\erc20 ->
                                            user
                                                |> User.hasEnoughAllowance
                                                    erc20
                                                    total
                                        )
                                    |> Maybe.withDefault True
                               )
                    then
                        ( modal
                            |> Modal
                            |> Just
                        , Cmd.none
                        , user
                            |> User.getDues
                            |> Remote.map (Dues.getMultiple modal.pool set)
                            |> (Remote.map << Maybe.map << Dict.map)
                                (\_ { debt } -> debt)
                            |> (Remote.map << Maybe.andThen)
                                (\assetsIn ->
                                    { pool = modal.pool
                                    , assetsIn = assetsIn
                                    }
                                        |> Pay
                                        |> Just
                                )
                            |> Remote.withDefault Nothing
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (modal |> noCmdAndEffect)

        ( ClickPay, Custom dict ) ->
            (case getTotalFromCustom user modal.pool dict of
                Ok total ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (modal.pool.pair |> Pair.toAsset)
                                total
                        )
                            && (modal.pool.pair
                                    |> Pair.toAsset
                                    |> Token.toERC20
                                    |> Maybe.map
                                        (\erc20 ->
                                            user
                                                |> User.hasEnoughAllowance
                                                    erc20
                                                    total
                                        )
                                    |> Maybe.withDefault True
                               )
                    then
                        ( modal
                            |> Modal
                            |> Just
                        , Cmd.none
                        , dict
                            |> Dict.foldr
                                (\tokenId assetIn accumulator ->
                                    accumulator
                                        |> Maybe.andThen
                                            (\accumulatedDict ->
                                                case
                                                    assetIn
                                                        |> Uint.fromAmount
                                                            (modal.pool.pair |> Pair.toAsset)
                                                of
                                                    Just assetInUint ->
                                                        accumulatedDict
                                                            |> Dict.insert tokenId assetInUint
                                                            |> Just

                                                    _ ->
                                                        Nothing
                                            )
                                )
                                (Dict.empty TokenId.sorter |> Just)
                            |> Maybe.map
                                (\assetsIn ->
                                    { pool = modal.pool
                                    , assetsIn = assetsIn
                                    }
                                        |> Pay
                                )
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (modal |> noCmdAndEffect)

        ( OnMouseEnter tooltip, _ ) ->
            { modal | tooltip = Just tooltip }
                |> noCmdAndEffect

        ( OnMouseLeave, _ ) ->
            { modal | tooltip = Nothing }
                |> noCmdAndEffect

        ( Exit, _ ) ->
            ( Nothing
            , Cmd.none
            , Nothing
            )

        _ ->
            modal |> noCmdAndEffect


getTotalFromFull : User -> Pool -> Set TokenId -> Result Error Uint
getTotalFromFull user pool set =
    user
        |> User.getDues
        |> Remote.map (Dues.getMultiple pool set)
        |> (Remote.map << Maybe.map)
            (\dict ->
                dict
                    |> Dict.foldl
                        (\_ { debt } accumulator ->
                            accumulator
                                |> Result.map
                                    (Uint.add debt)
                                |> Result.andThen (Result.fromMaybe Error.SumOverflow)
                        )
                        (Ok Uint.zero)
            )
        |> (Remote.map << Maybe.withDefault) (Err Error.Invalid)
        |> Remote.withDefault (Err Error.Invalid)


getTotalFromCustom : User -> Pool -> Dict TokenId String -> Result Error Uint
getTotalFromCustom user pool dict =
    user
        |> User.getDues
        |> Remote.map
            (dict
                |> Dict.keys
                |> Set.fromList TokenId.sorter
                |> Dues.getMultiple pool
            )
        |> (Remote.map << Maybe.map)
            (\dues ->
                Dict.merge TokenId.sorter
                    (\_ _ accumulator -> accumulator)
                    (\_ assetIn { debt, collateral } accumulator ->
                        Ok (\a b -> Uint.add a b |> Result.fromMaybe Error.SumOverflow)
                            |> Result.apply accumulator
                            |> Result.apply
                                (assetIn
                                    |> Uint.fromAmount (pool.pair |> Pair.toAsset)
                                    |> Result.fromMaybe Error.Invalid
                                    |> Result.andThen
                                        (\assetInUint ->
                                            Uint.proportion assetInUint collateral debt
                                                |> Maybe.map (\_ -> assetInUint)
                                                |> Result.fromMaybe Error.RepayOverflow
                                        )
                                )
                            |> Result.andThen identity
                    )
                    (\_ _ accumulator -> accumulator)
                    dict
                    dues
                    (Ok Uint.zero)
            )
        |> (Remote.map << Maybe.withDefault) (Err Error.Invalid)
        |> Remote.withDefault (Err Error.Invalid)


getTotal : User -> Pool -> State -> Result Error Uint
getTotal user pool state =
    case state of
        Full set ->
            getTotalFromFull user pool set

        Custom dict ->
            getTotalFromCustom user pool dict


noCmdAndEffect :
    { pool : Pool
    , state : State
    , tooltip : Maybe Tooltip
    }
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
noCmdAndEffect modal =
    ( modal
        |> Modal
        |> Just
    , Cmd.none
    , Nothing
    )


view :
    { model
        | backdrop : Backdrop
        , images : Images
        , theme : Theme
    }
    -> Blockchain
    -> User
    -> Modal
    -> Element Msg
view ({ backdrop, theme } as model) blockchain user (Modal modal) =
    Outside.view model
        { onClick = Exit
        , modal =
            column
                ([ width <| minimum 375 fill
                 , height shrink
                 , spacing 16
                 , centerX
                 , centerY
                 , Border.rounded 8
                 , theme |> ThemeColor.border |> Border.color
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop theme
                )
                [ column
                    [ width fill
                    , height shrink
                    ]
                    [ header model
                    , body model blockchain user modal
                    ]
                ]
        }


header :
    { model
        | theme : Theme
        , images : Images
    }
    -> Element Msg
header model =
    row
        [ width fill
        , height shrink
        , paddingEach
            { top = 24
            , right = 24
            , bottom = 20
            , left = 24
            }
        , Border.widthEach
            { top = 0
            , right = 0
            , bottom = 1
            , left = 0
            }
        , model.theme |> ThemeColor.textboxBorder |> Border.color
        ]
        [ el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 18
            , Font.bold
            , paddingXY 0 3
            , model.theme |> ThemeColor.text |> Font.color
            ]
            (text "Repay debt")
        , IconButton.exit model Exit
        ]


body :
    { model
        | theme : Theme
        , images : Images
    }
    -> Blockchain
    -> User
    ->
        { modal
            | pool : Pool
            , state : State
            , tooltip : Maybe Tooltip
        }
    -> Element Msg
body model blockchain user modal =
    column
        [ width fill
        , padding 20
        , spacing 16
        , height <| minimum 435 fill
        , scrollbarY
        ]
        [ row
            [ width fill
            , height <| px 44
            , spacing 16
            , padding 4
            , model.theme |> ThemeColor.border |> Background.color
            , Border.rounded 8
            ]
            [ switchRepayFull model modal
            , switchRepayCustom model modal
            ]
        , repayList model blockchain user modal
        , buttons model user modal
        ]


switchRepayFull :
    { model
        | theme : Theme
    }
    -> { modal | state : State }
    -> Element Msg
switchRepayFull { theme } { state } =
    Input.button
        ([ width fill
         , height <| px 36
         , paddingXY 12 8
         ]
            ++ (case state of
                    Full _ ->
                        [ theme |> ThemeColor.primaryBtn |> Background.color
                        , Border.rounded 4
                        ]

                    _ ->
                        []
               )
        )
        { onPress =
            case state of
                Full _ ->
                    Nothing

                Custom _ ->
                    Just (SwitchMode RepayFull)
        , label =
            el
                [ centerX
                , centerY
                , (case state of
                    Full _ ->
                        Color.light100

                    Custom _ ->
                        theme |> ThemeColor.textLight
                  )
                    |> Font.color
                , Font.size 16
                , Font.bold
                ]
                (text "Repay Full")
        }


switchRepayCustom :
    { model | theme : Theme }
    -> { modal | state : State }
    -> Element Msg
switchRepayCustom { theme } { state } =
    Input.button
        ([ width fill
         , height <| px 36
         , paddingXY 12 8
         ]
            ++ (case state of
                    Custom _ ->
                        [ theme |> ThemeColor.primaryBtn |> Background.color
                        , Border.rounded 4
                        ]

                    _ ->
                        []
               )
        )
        { onPress =
            case state of
                Full _ ->
                    Just (SwitchMode RepayCustom)

                Custom _ ->
                    Nothing
        , label =
            el
                [ centerX
                , centerY
                , (case state of
                    Full _ ->
                        theme |> ThemeColor.textLight

                    Custom _ ->
                        Color.light100
                  )
                    |> Font.color
                , Font.size 16
                , Font.bold
                ]
                (text "Repay Custom")
        }


repayList :
    { model
        | theme : Theme
        , images : Images
    }
    -> Blockchain
    -> User
    ->
        { modal
            | pool : Pool
            , state : State
            , tooltip : Maybe Tooltip
        }
    -> Element Msg
repayList model blockchain user ({ state, pool } as modal) =
    column
        [ width fill
        , spacing 12
        ]
        (case state of
            Full tokenIdSet ->
                user
                    |> User.getDues
                    |> Remote.map (Dues.getMultiple pool tokenIdSet)
                    |> (Remote.map << Maybe.map)
                        (\dict ->
                            (\list total ->
                                list ++ [ total ]
                            )
                                (dict
                                    |> Dict.toList
                                    |> List.map (fullPosition model blockchain user modal)
                                )
                                -- Show Total debt-collateral only if multiple positions are being repaid
                                (if (dict |> Dict.size) < 2 then
                                    none

                                 else
                                    dict
                                        |> Dict.foldl
                                            (\_ { debt, collateral } accumulator ->
                                                accumulator
                                                    |> Maybe.andThen
                                                        (\accumulatedDue ->
                                                            Just Due
                                                                |> Maybe.apply
                                                                    (Uint.add accumulatedDue.debt debt)
                                                                |> Maybe.apply
                                                                    (Uint.add accumulatedDue.collateral collateral)
                                                        )
                                            )
                                            (Due Uint.zero Uint.zero |> Just)
                                        |> totalDebtCollateral model modal
                                )
                        )
                    |> (Remote.map << Maybe.withDefault) []
                    |> Remote.withDefault []

            Custom dict ->
                user
                    |> User.getDues
                    |> Remote.map
                        (dict
                            |> Dict.keys
                            |> Set.fromList TokenId.sorter
                            |> Dues.getMultiple pool
                        )
                    |> (Remote.map << Maybe.map)
                        (\dues ->
                            (\list total ->
                                list ++ [ total ]
                            )
                                (List.map2 (customPosition model user modal)
                                    (dues |> Dict.toList)
                                    (dict |> Dict.values)
                                )
                                -- Show Total debt-collateral only if multiple positions are being repaid
                                (if (dict |> Dict.size) < 2 then
                                    none

                                 else
                                    Dict.merge TokenId.sorter
                                        (\_ _ accumulator -> accumulator)
                                        (\_ { debt, collateral } assetIn accumulator ->
                                            accumulator
                                                |> Maybe.andThen
                                                    (\accumulatedDue ->
                                                        Just Due
                                                            |> Maybe.apply
                                                                (assetIn
                                                                    |> Uint.fromAmount
                                                                        (pool.pair |> Pair.toAsset)
                                                                    |> Maybe.andThen (Uint.add accumulatedDue.debt)
                                                                )
                                                            |> Maybe.apply
                                                                (assetIn
                                                                    |> Uint.fromAmount
                                                                        (pool.pair |> Pair.toAsset)
                                                                    |> Maybe.andThen
                                                                        (\assetInUint ->
                                                                            Uint.proportion assetInUint collateral debt
                                                                        )
                                                                    |> Maybe.andThen (Uint.add accumulatedDue.collateral)
                                                                )
                                                    )
                                        )
                                        (\_ _ accumulator -> accumulator)
                                        dues
                                        dict
                                        (Due Uint.zero Uint.zero |> Just)
                                        |> totalDebtCollateral model modal
                                )
                        )
                    |> (Remote.map << Maybe.withDefault) []
                    |> Remote.withDefault []
        )


fullPosition :
    { model
        | theme : Theme
        , images : Images
    }
    -> Blockchain
    -> User
    ->
        { modal
            | pool : Pool
            , tooltip : Maybe Tooltip
        }
    -> ( TokenId, Due )
    -> Element Msg
fullPosition { theme, images } blockchain user { pool, tooltip } ( tokenId, due ) =
    column
        [ width fill
        , paddingXY 16 12
        , spacing 12
        , theme |> ThemeColor.sectionBackground |> Background.color
        , Border.rounded 8
        ]
        [ row
            [ width fill
            , spacing 6
            , centerY
            ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.textLight |> Font.color
                ]
                (text "Position ID")
            , el
                [ alignRight
                , theme |> ThemeColor.textLight |> Font.color
                , Font.size 14
                , Font.bold
                ]
                (text (tokenId |> TokenId.toString))
            , case user |> User.getPoolNatives pool of
                Success (Just natives) ->
                    newTabLink
                        [ alignRight ]
                        { url =
                            Builder.crossOrigin (blockchain |> Blockchain.toChain |> Chain.toNftExplorerUrl)
                                [ natives.collateralizedDebt |> Address.toString
                                , tokenId |> Uint.toString
                                ]
                                []
                        , label =
                            images
                                |> (case theme of
                                        Theme.Dark ->
                                            Image.link

                                        Theme.Light ->
                                            Image.linkSecondary
                                   )
                                    [ width <| px 14
                                    , height <| px 14
                                    ]
                        }

                _ ->
                    none
            ]
        , row
            [ width fill
            , spacing 6
            , centerY
            ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.textLight |> Font.color
                ]
                (text "Amount to repay")
            , el
                [ Font.size 14
                , Font.bold
                , alignRight
                , theme |> ThemeColor.text |> Font.color
                ]
                (Truncate.viewAmount
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.DebtAmount tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toAsset
                    , amount = due.debt
                    , theme = theme
                    , customStyles = [ Font.size 14 ]
                    }
                )
            , el
                [ alignRight
                , paddingXY 3 1
                , Border.rounded 4
                , theme |> ThemeColor.border |> Background.color
                ]
                (Truncate.viewSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.DebtSymbol tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toAsset
                    , theme = theme
                    , customStyles =
                        [ Font.size 14
                        , theme |> ThemeColor.textLight |> Font.color
                        , Font.regular
                        , paddingEach
                            { top = 4
                            , right = 0
                            , bottom = 2
                            , left = 0
                            }
                        ]
                    }
                )
            ]
        , row [ width fill, spacing 6, centerY ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.textLight |> Font.color
                ]
                (text "Collateral to unlock")
            , el
                [ Font.size 14
                , Font.bold
                , alignRight
                , theme |> ThemeColor.text |> Font.color
                ]
                (Truncate.viewAmount
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.CollateralAmount tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toCollateral
                    , amount = due.collateral
                    , theme = theme
                    , customStyles = [ Font.size 14 ]
                    }
                )
            , el
                [ alignRight
                , paddingXY 3 1
                , Border.rounded 4
                , theme |> ThemeColor.border |> Background.color
                ]
                (Truncate.viewSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.CollateralSymbol tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toCollateral
                    , theme = theme
                    , customStyles =
                        [ Font.size 14
                        , theme |> ThemeColor.textLight |> Font.color
                        , Font.regular
                        , paddingEach
                            { top = 4
                            , right = 0
                            , bottom = 2
                            , left = 0
                            }
                        ]
                    }
                )
            ]
        ]


totalDebtCollateral :
    { model
        | theme : Theme
        , images : Images
    }
    ->
        { modal
            | pool : Pool
            , tooltip : Maybe Tooltip
        }
    -> Maybe Due
    -> Element Msg
totalDebtCollateral { theme, images } { pool, tooltip } maybeDues =
    column
        [ width fill
        , spacing 12
        , paddingXY 0 4
        ]
        [ row
            [ width fill
            , centerY
            , spacing 4
            ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.actionElemLabel |> Font.color
                ]
                (text "Total amount to repay")
            , el [ alignRight ]
                (maybeDues
                    |> Maybe.map
                        (\{ debt } ->
                            Truncate.viewAmount
                                { onMouseEnter = OnMouseEnter
                                , onMouseLeave = OnMouseLeave
                                , tooltip = Tooltip.TotalDebt
                                , opened = tooltip
                                , token = pool.pair |> Pair.toAsset
                                , amount = debt
                                , theme = theme
                                , customStyles = [ Font.size 14, Font.bold ]
                                }
                        )
                    |> Maybe.withDefault none
                )
            , el []
                (images
                    |> Image.viewToken
                        [ width <| px 16, height <| px 16 ]
                        (pool.pair |> Pair.toAsset)
                )
            , el []
                (Truncate.viewSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.TotalDebtSymbol
                    , opened = tooltip
                    , token = pool.pair |> Pair.toAsset
                    , theme = theme
                    , customStyles =
                        [ Font.size 14
                        , Font.bold
                        , theme |> ThemeColor.text |> Font.color
                        ]
                    }
                )
            ]
        , row
            [ width fill
            , centerY
            , spacing 4
            ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.actionElemLabel |> Font.color
                ]
                (text "Total collateral to unlock")
            , el [ alignRight ]
                (maybeDues
                    |> Maybe.map
                        (\{ collateral } ->
                            Truncate.viewAmount
                                { onMouseEnter = OnMouseEnter
                                , onMouseLeave = OnMouseLeave
                                , tooltip = Tooltip.TotalCollateral
                                , opened = tooltip
                                , token = pool.pair |> Pair.toCollateral
                                , amount = collateral
                                , theme = theme
                                , customStyles = [ Font.size 14, Font.bold ]
                                }
                        )
                    |> Maybe.withDefault none
                )
            , el []
                (images
                    |> Image.viewToken
                        [ width <| px 16, height <| px 16 ]
                        (pool.pair |> Pair.toCollateral)
                )
            , el []
                (Truncate.viewSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.TotalCollateralSymbol
                    , opened = tooltip
                    , token = pool.pair |> Pair.toCollateral
                    , theme = theme
                    , customStyles =
                        [ Font.size 14
                        , Font.bold
                        , theme |> ThemeColor.text |> Font.color
                        ]
                    }
                )
            ]
        ]


customPosition :
    { model
        | theme : Theme
        , images : Images
    }
    -> User
    ->
        { modal
            | pool : Pool
            , tooltip : Maybe Tooltip
        }
    -> ( TokenId, Due )
    -> String
    -> Element Msg
customPosition ({ theme, images } as model) user { pool, tooltip } ( tokenId, due ) assetIn =
    column
        [ width fill
        , paddingXY 16 14
        , spacing 12
        , theme |> ThemeColor.sectionBackground |> Background.color
        , Border.rounded 8
        ]
        [ row [ width fill, spacing 6, centerY ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.textLight |> Font.color
                ]
                (text "Position ID")
            , el
                [ alignRight
                , theme |> ThemeColor.textLight |> Font.color
                , Font.size 14
                , Font.bold
                ]
                (text (tokenId |> TokenId.toString))
            , images
                |> (case theme of
                        Theme.Dark ->
                            Image.link

                        Theme.Light ->
                            Image.linkSecondary
                   )
                    [ width <| px 14
                    , height <| px 14
                    ]
            ]
        , row
            [ width fill
            , spacing 6
            , centerY
            ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.textLight |> Font.color
                ]
                (text "Total debt to pay")
            , el
                [ Font.size 14
                , Font.bold
                , alignRight
                , theme |> ThemeColor.textLight |> Font.color
                ]
                (Truncate.viewAmount
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.DebtAmount tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toAsset
                    , amount = due.debt
                    , theme = theme
                    , customStyles = [ Font.size 14 ]
                    }
                )
            , el
                [ alignRight
                , paddingXY 3 1
                , Border.rounded 4
                , theme |> ThemeColor.border |> Background.color
                ]
                (Truncate.viewSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.DebtSymbol tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toAsset
                    , theme = theme
                    , customStyles =
                        [ Font.size 14
                        , theme |> ThemeColor.textLight |> Font.color
                        , Font.regular
                        , paddingEach
                            { top = 4
                            , right = 0
                            , bottom = 2
                            , left = 0
                            }
                        ]
                    }
                )
            ]
        , row
            [ width fill
            , spacing 8
            , centerY
            ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.actionElemLabel |> Font.color
                ]
                (text "Enter amount to repay")
            , user
                |> User.getBalance (pool.pair |> Pair.toAsset)
                |> Maybe.map
                    (\balance ->
                        MaxButton.view
                            { onPress = InputMax tokenId
                            , onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , tooltip = Tooltip.Balance tokenId
                            , opened = tooltip
                            , token = pool.pair |> Pair.toAsset
                            , balance = balance
                            , theme = model.theme
                            }
                    )
                |> Maybe.withDefault none
            ]
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.TextboxToken tokenId
            , opened = tooltip
            , token = pool.pair |> Pair.toAsset
            , onClick = Nothing
            , onChange = InputAssetIn tokenId
            , text = assetIn |> Left
            , description = "asset in textbox"
            }
        , row [ width fill, spacing 6, centerY ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.textLight |> Font.color
                ]
                (text "Collateral to unlock")
            , assetIn
                |> Uint.fromAmount (pool.pair |> Pair.toAsset)
                |> Maybe.andThen
                    (\assetInUint -> Uint.proportion assetInUint due.collateral due.debt)
                |> Maybe.map
                    (\collateralOut ->
                        el
                            [ Font.size 14
                            , Font.bold
                            , alignRight
                            , theme |> ThemeColor.text |> Font.color
                            ]
                            (Truncate.viewAmount
                                { onMouseEnter = OnMouseEnter
                                , onMouseLeave = OnMouseLeave
                                , tooltip = Tooltip.CollateralAmount tokenId
                                , opened = tooltip
                                , token = pool.pair |> Pair.toCollateral
                                , amount = collateralOut
                                , theme = theme
                                , customStyles = [ Font.size 14 ]
                                }
                            )
                    )
                |> Maybe.withDefault none

            -- Fix this soon
            , el
                [ alignRight
                , paddingXY 3 1
                , Border.rounded 4
                , theme |> ThemeColor.border |> Background.color
                ]
                (Truncate.viewSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.CollateralSymbol tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toCollateral
                    , theme = theme
                    , customStyles =
                        [ Font.size 14
                        , theme |> ThemeColor.textLight |> Font.color
                        , Font.regular
                        , paddingEach
                            { top = 4
                            , right = 0
                            , bottom = 2
                            , left = 0
                            }
                        ]
                    }
                )
            ]
        ]


buttons :
    { model | theme : Theme }
    -> User
    ->
        { modal
            | pool : Pool
            , state : State
        }
    -> Element Msg
buttons ({ theme } as model) user { pool, state } =
    case
        ( getTotal user pool state
        , pool.pair |> Pair.toAsset |> Token.toERC20
        )
    of
        ( Ok assetIn, Just erc20 ) ->
            case
                ( user
                    |> User.getBalance (pool.pair |> Pair.toAsset)
                    |> (Maybe.map << Remote.map)
                        (Uint.hasEnough assetIn)
                , user
                    |> User.getAllowance erc20
                    |> (Maybe.map << Remote.map)
                        (Uint.hasEnough assetIn)
                )
            of
                ( Just (Success True), Just (Success True) ) ->
                    confirmBtn model

                ( Just (Success False), Just (Success True) ) ->
                    Button.notEnoughBalance

                ( Just (Success True), Just (Success False) ) ->
                    approveButton erc20 theme

                ( Just (Loading _), Just (Success True) ) ->
                    theme |> Button.checkingBalance |> map never

                ( Just (Failure error), _ ) ->
                    Button.error error |> map never

                ( _, Just (Failure error) ) ->
                    Button.error error |> map never

                _ ->
                    disabledRepay theme

        ( Ok assetIn, Nothing ) ->
            case
                user
                    |> User.getBalance (pool.pair |> Pair.toAsset)
                    |> (Maybe.map << Remote.map)
                        (Uint.hasEnough assetIn)
            of
                Just (Success True) ->
                    confirmBtn model

                Just (Success False) ->
                    Button.notEnoughBalance

                Just (Loading _) ->
                    theme |> Button.checkingBalance |> map never

                Just (Failure error) ->
                    Button.error error |> map never

                _ ->
                    disabledRepay theme

        ( Err error, _ ) ->
            error |> errorBtn |> map never


errorBtn : Error -> Element Never
errorBtn error =
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
            (error |> Error.toString |> text)
        )


approveButton : ERC20 -> Theme -> Element Msg
approveButton erc20 theme =
    Button.approve
        { onPress = ClickApprove
        , erc20 = erc20
        , theme = theme
        }


disabledRepay : Theme -> Element msg
disabledRepay theme =
    Button.disabled theme "Repay"
        |> map never


confirmBtn :
    { model
        | theme : Theme
    }
    -> Element Msg
confirmBtn { theme } =
    Input.button
        [ width fill
        , centerY
        ]
        { onPress = Just ClickPay
        , label =
            el
                [ width fill
                , height <| px 44
                , centerX
                , centerY
                , paddingXY 12 14
                , Border.rounded 4
                , Font.size 16
                , Font.bold
                , Font.color Color.light100
                , Font.center
                , theme |> ThemeColor.primaryBtn |> Background.color
                ]
                (text "Confirm Repay")
        }
