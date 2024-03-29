module Utility.Image exposing
    ( allPairs
    , arrow
    , arrowDownDark
    , arrowLeft
    , arrowLeftDark
    , arrowSecondary
    , borrowloadingPositionsIcon
    , borrowloadingPositionsIconDark
    , calendar
    , checkbox
    , checkboxSelected
    , circleGreen
    , close
    , closeDark
    , copy
    , default
    , discloser
    , discord
    , energy
    , error
    , faucet
    , gitbook
    , github
    , hourglass
    , hourglassDark
    , hourglassPrimary
    , hourglassPrimarySmall
    , info
    , infoDark
    , lendloadingPositionsIcon
    , lendloadingPositionsIconDark
    , link
    , linkDark
    , linkSecondary
    , linkWhite
    , loading
    , loadingAnimation
    , loadingFailed
    , loadingPositions
    , loadingPositionsDark
    , loadingSuccess
    , logoPure
    , logoText
    , logoTextDark
    , matured
    , medium
    , mediumSecondary
    , metamask
    , minus
    , minusDark
    , moon
    , openSea
    , option
    , plus
    , plusPositive
    , questionMark
    , questionMarkDark
    , semiCircleGreen
    , semiCircleRed
    , semiCircleYellow
    , setting
    , settingsSecondary
    , sun
    , telegram
    , tripleDots
    , twitter
    , video
    , viewChain
    , viewToken
    , viewWallet
    , wallet
    , warning
    , warningCircle
    )

import Data.Chain exposing (Chain(..))
import Data.ERC20 as ERC20
import Data.Images exposing (Images)
import Data.Native as Native
import Data.Token as Token exposing (Token)
import Data.Wallet as Wallet exposing (Wallet)
import Element
    exposing
        ( Attribute
        , Element
        , image
        )
import Sort.Dict as Dict


view : String -> List (Attribute msg) -> Images -> Element msg
view name attributes images =
    image
        attributes
        { src =
            images
                |> .images
                |> Dict.get name
                |> Maybe.withDefault ""
        , description = name
        }


viewGifs : String -> List (Attribute msg) -> Images -> Element msg
viewGifs name attributes images =
    image
        attributes
        { src =
            images
                |> .gifs
                |> Dict.get name
                |> Maybe.withDefault ""
        , description = name
        }


viewToken : List (Attribute msg) -> Token -> Images -> Element msg
viewToken attributes token images =
    (case token of
        Token.Native native ->
            native |> Native.toSymbol

        Token.ERC20 erc20 ->
            erc20 |> ERC20.toSymbol
    )
        |> (\symbol ->
                image
                    attributes
                    { src =
                        images
                            |> .tokens
                            |> Dict.get symbol
                            |> Maybe.withDefault (images |> defaultTokenUrl)
                    , description = symbol
                    }
           )


defaultTokenUrl : Images -> String
defaultTokenUrl images =
    images
        |> .tokens
        |> Dict.get "default"
        |> Maybe.withDefault ""


viewChain : List (Attribute msg) -> Chain -> Images -> Element msg
viewChain attributes (Chain { name }) images =
    image
        attributes
        { src =
            images
                |> .chains
                |> Dict.get name
                |> Maybe.withDefault "default"
        , description = name
        }


viewWallet : List (Attribute msg) -> Wallet -> Images -> Element msg
viewWallet attributes givenWallet images =
    image
        attributes
        { src =
            images
                |> .wallets
                |> Dict.get
                    (givenWallet
                        |> Wallet.toString
                    )
                |> Maybe.withDefault "default"
        , description =
            givenWallet
                |> Wallet.toString
        }


default : List (Attribute msg) -> Images -> Element msg
default =
    view "Default"


error : List (Attribute msg) -> Images -> Element msg
error =
    view "Error"


copy : List (Attribute msg) -> Images -> Element msg
copy =
    view "Copy"


plusPositive : List (Attribute msg) -> Images -> Element msg
plusPositive =
    view "PlusPositive"


circleGreen : List (Attribute msg) -> Images -> Element msg
circleGreen =
    view "CircleGreen"


semiCircleGreen : List (Attribute msg) -> Images -> Element msg
semiCircleGreen =
    view "SemiCircleGreen"


semiCircleRed : List (Attribute msg) -> Images -> Element msg
semiCircleRed =
    view "SemiCircleRed"


semiCircleYellow : List (Attribute msg) -> Images -> Element msg
semiCircleYellow =
    view "SemiCircleYellow"


sun : List (Attribute msg) -> Images -> Element msg
sun =
    view "Sun"


moon : List (Attribute msg) -> Images -> Element msg
moon =
    view "Moon"


openSea : List (Attribute msg) -> Images -> Element msg
openSea =
    view "OpenSea"


logoText : List (Attribute msg) -> Images -> Element msg
logoText =
    view "LogoText"


logoTextDark : List (Attribute msg) -> Images -> Element msg
logoTextDark =
    view "LogoTextDark"


logoPure : List (Attribute msg) -> Images -> Element msg
logoPure =
    view "LogoPure"


metamask : List (Attribute msg) -> Images -> Element msg
metamask =
    view "Metamask"


link : List (Attribute msg) -> Images -> Element msg
link =
    view "Link"


linkSecondary : List (Attribute msg) -> Images -> Element msg
linkSecondary =
    view "LinkSecondary"


linkWhite : List (Attribute msg) -> Images -> Element msg
linkWhite =
    view "LinkWhite"


linkDark : List (Attribute msg) -> Images -> Element msg
linkDark =
    view "LinkDark"


energy : List (Attribute msg) -> Images -> Element msg
energy =
    view "Energy"


option : List (Attribute msg) -> Images -> Element msg
option =
    view "Option"


allPairs : List (Attribute msg) -> Images -> Element msg
allPairs =
    view "AllPairs"


wallet : List (Attribute msg) -> Images -> Element msg
wallet =
    view "Wallet"


faucet : List (Attribute msg) -> Images -> Element msg
faucet =
    view "Faucet"


discloser : List (Attribute msg) -> Images -> Element msg
discloser =
    view "Discloser"


hourglass : List (Attribute msg) -> Images -> Element msg
hourglass =
    view "Hourglass"


hourglassPrimary : List (Attribute msg) -> Images -> Element msg
hourglassPrimary =
    view "HourglassPrimary"


hourglassPrimarySmall : List (Attribute msg) -> Images -> Element msg
hourglassPrimarySmall =
    view "HourglassPrimarySmall"


hourglassDark : List (Attribute msg) -> Images -> Element msg
hourglassDark =
    view "HourglassDark"


plus : List (Attribute msg) -> Images -> Element msg
plus =
    view "Plus"


minus : List (Attribute msg) -> Images -> Element msg
minus =
    view "Minus"


minusDark : List (Attribute msg) -> Images -> Element msg
minusDark =
    view "MinusDark"


arrow : List (Attribute msg) -> Images -> Element msg
arrow =
    view "Arrow"


arrowSecondary : List (Attribute msg) -> Images -> Element msg
arrowSecondary =
    view "ArrowSecondary"


close : List (Attribute msg) -> Images -> Element msg
close =
    view "Close"


closeDark : List (Attribute msg) -> Images -> Element msg
closeDark =
    view "CloseDark"


arrowLeft : List (Attribute msg) -> Images -> Element msg
arrowLeft =
    view "ArrowLeft"


arrowLeftDark : List (Attribute msg) -> Images -> Element msg
arrowLeftDark =
    view "ArrowLeftDark"


arrowDownDark : List (Attribute msg) -> Images -> Element msg
arrowDownDark =
    view "ArrowDownDark"


info : List (Attribute msg) -> Images -> Element msg
info =
    view "Info"


infoDark : List (Attribute msg) -> Images -> Element msg
infoDark =
    view "InfoDark"


warningCircle : List (Attribute msg) -> Images -> Element msg
warningCircle =
    view "WarningCircle"


matured : List (Attribute msg) -> Images -> Element msg
matured =
    view "Matured"


calendar : List (Attribute msg) -> Images -> Element msg
calendar =
    view "Calendar"


setting : List (Attribute msg) -> Images -> Element msg
setting =
    view "Setting"


settingsSecondary : List (Attribute msg) -> Images -> Element msg
settingsSecondary =
    view "SettingsSecondary"


checkbox : List (Attribute msg) -> Images -> Element msg
checkbox =
    view "Checkbox"


checkboxSelected : List (Attribute msg) -> Images -> Element msg
checkboxSelected =
    view "CheckboxSelected"


warning : List (Attribute msg) -> Images -> Element msg
warning =
    view "Warning"


tripleDots : List (Attribute msg) -> Images -> Element msg
tripleDots =
    view "TripleDots"


video : List (Attribute msg) -> Images -> Element msg
video =
    view "Video"


questionMark : List (Attribute msg) -> Images -> Element msg
questionMark =
    view "QuestionMark"


questionMarkDark : List (Attribute msg) -> Images -> Element msg
questionMarkDark =
    view "QuestionMarkDark"


gitbook : List (Attribute msg) -> Images -> Element msg
gitbook =
    view "Gitbook"


github : List (Attribute msg) -> Images -> Element msg
github =
    view "Github"


discord : List (Attribute msg) -> Images -> Element msg
discord =
    view "Discord"


twitter : List (Attribute msg) -> Images -> Element msg
twitter =
    view "Twitter"


telegram : List (Attribute msg) -> Images -> Element msg
telegram =
    view "Telegram"


medium : List (Attribute msg) -> Images -> Element msg
medium =
    view "Medium"


mediumSecondary : List (Attribute msg) -> Images -> Element msg
mediumSecondary =
    view "MediumSecondary"


loadingPositions : List (Attribute msg) -> Images -> Element msg
loadingPositions =
    view "LoadingPositions"


loadingPositionsDark : List (Attribute msg) -> Images -> Element msg
loadingPositionsDark =
    view "LoadingPositionsDark"


lendloadingPositionsIcon : List (Attribute msg) -> Images -> Element msg
lendloadingPositionsIcon =
    view "LendEmpty"


lendloadingPositionsIconDark : List (Attribute msg) -> Images -> Element msg
lendloadingPositionsIconDark =
    view "LendEmptyDark"


borrowloadingPositionsIcon : List (Attribute msg) -> Images -> Element msg
borrowloadingPositionsIcon =
    view "BorrowEmpty"


borrowloadingPositionsIconDark : List (Attribute msg) -> Images -> Element msg
borrowloadingPositionsIconDark =
    view "BorrowEmptyDark"


loading : List (Attribute msg) -> Images -> Element msg
loading =
    viewGifs "Loading"


loadingFailed : List (Attribute msg) -> Images -> Element msg
loadingFailed =
    viewGifs "LoadingFailed"


loadingSuccess : List (Attribute msg) -> Images -> Element msg
loadingSuccess =
    viewGifs "LoadingSuccess"


loadingAnimation : List (Attribute msg) -> Images -> Element msg
loadingAnimation =
    viewGifs "LoadingAnimation"
