module Data.Device exposing (Device(..), fromWidth, isPhoneOrTablet)


type Device
    = Phone
    | Tablet
    | Desktop
    | BigDesktop


fromWidth : Int -> Device
fromWidth width =
    if width < 600 then
        Phone

    else if width < 1080 + 278 then
        Tablet

    else if width < 1366 + 278 then
        Desktop

    else
        BigDesktop


isPhoneOrTablet : Device -> Bool
isPhoneOrTablet device =
    case device of
        Phone ->
            True

        Tablet ->
            True

        _ ->
            False
