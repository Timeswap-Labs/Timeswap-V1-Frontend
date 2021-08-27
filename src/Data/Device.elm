module Data.Device exposing (Device, checkAsideStatus, closeAside, fromDeviceWidth, fromViewport, fromWidth, isPhone, isPhoneOrTablet, isTablet, openAside)


type Device
    = Phone AsideStatus
    | Tablet AsideStatus
    | Desktop
    | BigDesktop


type AsideStatus
    = Open
    | Close


fromViewport : { viewport | width : Int } -> Device
fromViewport { width } =
    fromWidth width


fromWidth : Int -> Device
fromWidth width =
    if width < 600 then
        Phone Close

    else if width < 1080 + 278 then
        Tablet Close

    else if width < 1366 + 278 then
        Desktop

    else
        BigDesktop


fromDeviceWidth : Device -> Int -> Device
fromDeviceWidth device width =
    let
        newDevice : Device
        newDevice =
            fromWidth width
    in
    if checkAsideStatus device then
        toggleAside newDevice

    else
        newDevice


openAside : Device -> Device
openAside device =
    case device of
        Phone _ ->
            Phone Open

        Tablet _ ->
            Tablet Open

        _ ->
            device


closeAside : Device -> Device
closeAside device =
    case device of
        Phone _ ->
            Phone Close

        Tablet _ ->
            Tablet Close

        _ ->
            device


isPhone : Device -> Bool
isPhone device =
    case device of
        Phone _ ->
            True

        _ ->
            False


isTablet : Device -> Bool
isTablet device =
    case device of
        Tablet _ ->
            True

        _ ->
            False


isPhoneOrTablet : Device -> Bool
isPhoneOrTablet device =
    isPhone device || isTablet device


isDesktop : Device -> Bool
isDesktop device =
    case device of
        Desktop ->
            True

        _ ->
            False


isBigDesktop : Device -> Bool
isBigDesktop device =
    case device of
        BigDesktop ->
            True

        _ ->
            False


checkAsideStatus : Device -> Bool
checkAsideStatus device =
    case device of
        Phone Open ->
            True

        Tablet Open ->
            True

        _ ->
            False


toggleAside : Device -> Device
toggleAside device =
    case device of
        Phone Open ->
            Phone Close

        Phone Close ->
            Phone Open

        Tablet Open ->
            Tablet Close

        Tablet Close ->
            Tablet Open

        _ ->
            device
