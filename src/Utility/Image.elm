module Utility.Image exposing
    ( allPairs
    , arrow
    , arrowDown
    , blackSun
    , calander
    , checkbox
    , checkboxSelected
    , close
    , copy
    , discloser
    , discord
    , energy
    , faucet
    , gitbook
    , github
    , hourglass
    , hourglassPrimary
    , hourglassPrimarySmall
    , info
    , infoYellow
    , link
    , logo
    , logoPure
    , matured
    , medium
    , metamask
    , minus
    , openSea
    , option
    , plus
    , plusPositive
    , setting
    , telegram
    , tripleDots
    , twitter
    , viewChain
    , viewToken
    , viewWallet
    , wallet
    , warning
    , whiteSun
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
                            |> Maybe.withDefault "default"
                    , description = symbol
                    }
           )


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


copy : List (Attribute msg) -> Images -> Element msg
copy =
    view "Copy"


plusPositive : List (Attribute msg) -> Images -> Element msg
plusPositive =
    view "PlusPositive"


whiteSun : List (Attribute msg) -> Images -> Element msg
whiteSun =
    view "WhiteSun"


blackSun : List (Attribute msg) -> Images -> Element msg
blackSun =
    view "BlackSun"


openSea : List (Attribute msg) -> Images -> Element msg
openSea =
    view "OpenSea"


logo : List (Attribute msg) -> Images -> Element msg
logo =
    view "Logo"


logoPure : List (Attribute msg) -> Images -> Element msg
logoPure =
    view "LogoPure"


metamask : List (Attribute msg) -> Images -> Element msg
metamask =
    view "Metamask"


link : List (Attribute msg) -> Images -> Element msg
link =
    view "Link"


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


hourglassPrimary : List (Attribute msg) -> Images -> Element msg
hourglassPrimary =
    view "HourglassPrimary"


hourglassPrimarySmall : List (Attribute msg) -> Images -> Element msg
hourglassPrimarySmall =
    view "HourglassPrimarySmall"


plus : List (Attribute msg) -> Images -> Element msg
plus =
    view "Plus"


minus : List (Attribute msg) -> Images -> Element msg
minus =
    view "Minus"


arrow : List (Attribute msg) -> Images -> Element msg
arrow =
    view "Arrow"


close : List (Attribute msg) -> Images -> Element msg
close =
    view "Close"


arrowDown : List (Attribute msg) -> Images -> Element msg
arrowDown =
    view "ArrowDown"


hourglass : List (Attribute msg) -> Images -> Element msg
hourglass =
    view "Hourglass"


info : List (Attribute msg) -> Images -> Element msg
info =
    view "Info"


infoYellow : List (Attribute msg) -> Images -> Element msg
infoYellow =
    view "InfoYellow"


matured : List (Attribute msg) -> Images -> Element msg
matured =
    view "Matured"


calander : List (Attribute msg) -> Images -> Element msg
calander =
    view "Calendar"


setting : List (Attribute msg) -> Images -> Element msg
setting =
    view "Setting"


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
