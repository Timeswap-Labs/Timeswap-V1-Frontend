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
    , energy
    , faucet
    , hourglass
    , hourglassPrimary
    , hourglassPrimarySmall
    , info
    , link
    , logo
    , logoPure
    , matured
    , metamask
    , option
    , plus
    , setting
    , token
    , wallet
    , warning
    )

import Element
    exposing
        ( Attribute
        , Element
        , image
        )


logo : List (Attribute msg) -> Element msg
logo attributes =
    image
        attributes
        { src = "./../../image/Logo.svg"
        , description = "Timeswap Logo"
        }


logoPure : List (Attribute msg) -> Element msg
logoPure attributes =
    image
        attributes
        { src = "./../../image/LogoPure.svg"
        , description = "Timeswap Logo"
        }


metamask : List (Attribute msg) -> Element msg
metamask attributes =
    image
        attributes
        { src = "./../../image/Metamask.svg"
        , description = "Metamask Icon"
        }


link : List (Attribute msg) -> Element msg
link attributes =
    image
        attributes
        { src = "./../../image/Link.svg"
        , description = "Link Icon"
        }


energy : List (Attribute msg) -> Element msg
energy attributes =
    image
        attributes
        { src = "./../../image/Energy.svg"
        , description = "Energy Icon"
        }


option : List (Attribute msg) -> Element msg
option attributes =
    image
        attributes
        { src = "./../../image/Option.svg"
        , description = "Option Icon"
        }


allPairs : List (Attribute msg) -> Element msg
allPairs attributes =
    image
        attributes
        { src = "./../../image/AllPairs.svg"
        , description = "AllPairs Icon"
        }


token : List (Attribute msg) -> Element msg
token attributes =
    image
        attributes
        { src = "./../../image/Token.svg"
        , description = "Sample Token"
        }


wallet : List (Attribute msg) -> Element msg
wallet attributes =
    image
        attributes
        { src = "./../../image/Wallet.svg"
        , description = "Wallet Icon"
        }


faucet : List (Attribute msg) -> Element msg
faucet attributes =
    image
        attributes
        { src = "./../../image/Faucet.svg"
        , description = "Faucet Icon"
        }


discloser : List (Attribute msg) -> Element msg
discloser attributes =
    image
        attributes
        { src = "./../../image/Discloser.svg"
        , description = "Discloser Icon"
        }


hourglassPrimary : List (Attribute msg) -> Element msg
hourglassPrimary attributes =
    image
        attributes
        { src = "./../../image/HourglassPrimary.svg"
        , description = "HourglassPrimary Icon"
        }


hourglassPrimarySmall : List (Attribute msg) -> Element msg
hourglassPrimarySmall attributes =
    image
        attributes
        { src = "./../../image/HourglassPrimarySmall.svg"
        , description = "HourglassPrimarySmall Icon"
        }


plus : List (Attribute msg) -> Element msg
plus attributes =
    image
        attributes
        { src = "./../../image/Plus.svg"
        , description = "Plus Icon"
        }


arrow : List (Attribute msg) -> Element msg
arrow attributes =
    image
        attributes
        { src = "./../../image/Arrow.svg"
        , description = "Arrow Icon"
        }


close : List (Attribute msg) -> Element msg
close attributes =
    image
        attributes
        { src = "./../../image/Close.svg"
        , description = "Close Icon"
        }


arrowDown : List (Attribute msg) -> Element msg
arrowDown attributes =
    image
        attributes
        { src = "./../../image/ArrowDown.svg"
        , description = "ArrowDown Icon"
        }


hourglass : List (Attribute msg) -> Element msg
hourglass attributes =
    image
        attributes
        { src = "./../../image/Hourglass.svg"
        , description = "Hourglass Icon"
        }


info : List (Attribute msg) -> Element msg
info attributes =
    image
        attributes
        { src = "./../../image/Info.svg"
        , description = "Info Icon"
        }


matured : List (Attribute msg) -> Element msg
matured attributes =
    image
        attributes
        { src = "./../../image/Matured.svg"
        , description = "Matured Icon"
        }


calander : List (Attribute msg) -> Element msg
calander attributes =
    image
        attributes
        { src = "./../../image/Calander.svg"
        , description = "Calander Icon"
        }


setting : List (Attribute msg) -> Element msg
setting attributes =
    image
        attributes
        { src = "./../../image/Setting.svg"
        , description = "Setting"
        }


checkbox : List (Attribute msg) -> Element msg
checkbox attributes =
    image
        attributes
        { src = "./../../image/Checkbox.svg"
        , description = "Checkbox"
        }


checkboxSelected : List (Attribute msg) -> Element msg
checkboxSelected attributes =
    image
        attributes
        { src = "./../../image/CheckboxSelected.svg"
        , description = "CheckboxSelected"
        }


warning : List (Attribute msg) -> Element msg
warning attributes =
    image
        attributes
        { src = "./../../image/Warning.svg"
        , description = "Warning"
        }


background : List (Attribute msg) -> Element msg
background attributes =
    image
        attributes
        { src = "./../../image/Background.svg"
        , description = "Background"
        }
