module Data.Device exposing (Device(..), fromWidth, isPhoneOrTablet)


type Device
    = Phone
    | Tablet
    | Desktop


fromWidth : Int -> Device
fromWidth width =
    if width < 875 then
        Phone

    else if width < 1185 then
        Tablet

    else
        Desktop


isPhoneOrTablet : Device -> Bool
isPhoneOrTablet device =
    case device of
        Phone ->
            True

        Tablet ->
            True

        _ ->
            False
