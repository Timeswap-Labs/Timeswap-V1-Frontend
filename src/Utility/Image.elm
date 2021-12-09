module Utility.Image exposing
    ( allPairs
    , arrow
    , arrowDown
    , background
    , calander
    , checkbox
    , checkboxSelected
    , close
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
    , link
    , logo
    , logoPure
    , matured
    , medium
    , metamask
    , openSea
    , option
    , plus
    , setting
    , telegram
    , token
    , tripleDots
    , twitter
    , wallet
    , warning
    )

import Data.Images exposing (Images)
import Element
    exposing
        ( Attribute
        , Element
        , el
        , fill
        , height
        , image
        , none
        , width
        )
import Element.Background as Background
import Sort.Dict as Dict


view : String -> Images -> List (Attribute msg) -> Element msg
view name dict attributes =
    image
        attributes
        { src =
            dict
                |> Dict.get name
                |> Maybe.withDefault ""
        , description = name
        }


background : Images -> Element msg
background images =
    el
        ([ width fill
         , height fill
         ]
            ++ (images
                    |> Dict.get "Background"
                    |> Maybe.map Background.image
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
               )
        )
        none


openSea : Images -> List (Attribute msg) -> Element msg
openSea =
    view "OpenSea"


logo : Images -> List (Attribute msg) -> Element msg
logo =
    view "Logo"


logoPure : Images -> List (Attribute msg) -> Element msg
logoPure =
    view "LogoPure"


metamask : Images -> List (Attribute msg) -> Element msg
metamask =
    view "Metamask"


link : Images -> List (Attribute msg) -> Element msg
link =
    view "Link"


energy : Images -> List (Attribute msg) -> Element msg
energy =
    view "Energy"


option : Images -> List (Attribute msg) -> Element msg
option =
    view "Option"


allPairs : Images -> List (Attribute msg) -> Element msg
allPairs =
    view "AllPairs"


token : Images -> List (Attribute msg) -> Element msg
token =
    view "Token"


wallet : Images -> List (Attribute msg) -> Element msg
wallet =
    view "Wallet"


faucet : Images -> List (Attribute msg) -> Element msg
faucet =
    view "Faucet"


discloser : Images -> List (Attribute msg) -> Element msg
discloser =
    view "Discloser"


hourglassPrimary : Images -> List (Attribute msg) -> Element msg
hourglassPrimary =
    view "HourglassPrimary"


hourglassPrimarySmall : Images -> List (Attribute msg) -> Element msg
hourglassPrimarySmall =
    view "HourglassPrimarySmall"


plus : Images -> List (Attribute msg) -> Element msg
plus =
    view "Plus"


arrow : Images -> List (Attribute msg) -> Element msg
arrow =
    view "Arrow"


close : Images -> List (Attribute msg) -> Element msg
close =
    view "Close"


arrowDown : Images -> List (Attribute msg) -> Element msg
arrowDown =
    view "ArrowDown"


hourglass : Images -> List (Attribute msg) -> Element msg
hourglass =
    view "Hourglass"


info : Images -> List (Attribute msg) -> Element msg
info =
    view "Info"


matured : Images -> List (Attribute msg) -> Element msg
matured =
    view "Matured"


calander : Images -> List (Attribute msg) -> Element msg
calander =
    view "Calendar"


setting : Images -> List (Attribute msg) -> Element msg
setting =
    view "Setting"


checkbox : Images -> List (Attribute msg) -> Element msg
checkbox =
    view "Checkbox"


checkboxSelected : Images -> List (Attribute msg) -> Element msg
checkboxSelected =
    view "CheckboxSelected"


warning : Images -> List (Attribute msg) -> Element msg
warning =
    view "Warning"


tripleDots : Images -> List (Attribute msg) -> Element msg
tripleDots =
    view "TripleDots"


gitbook : Images -> List (Attribute msg) -> Element msg
gitbook =
    view "Gitbook"


github : Images -> List (Attribute msg) -> Element msg
github =
    view "Github"


discord : Images -> List (Attribute msg) -> Element msg
discord =
    view "Discord"


twitter : Images -> List (Attribute msg) -> Element msg
twitter =
    view "Twitter"


telegram : Images -> List (Attribute msg) -> Element msg
telegram =
    view "Telegram"


medium : Images -> List (Attribute msg) -> Element msg
medium =
    view "Medium"
