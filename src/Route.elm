module Route exposing (Route(..), fromUrl, pushUrl)

import Aside
import Browser.Navigation as Navigation exposing (Key)
import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
import Data.Pools exposing (Pools)
import Data.Positions exposing (Positions)
import Data.Remote exposing (Remote)
import Data.Tokens exposing (Tokens)
import Modal exposing (Modal)
import Page exposing (Page)
import Service exposing (Service)
import Time exposing (Posix)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)
import Utility.Router as Router


type Route
    = Page Page
    | Modal Modal
    | Service Service
    | Aside
    | Exit


pushUrl :
    { model
        | device : Device
        , time : Posix
        , key : Key
        , tokens : Tokens
        , pools : Pools
        , user : Maybe { user | chain : Chain, positions : Remote Positions }
        , page : Page
        , modal : Maybe Modal
        , service : Maybe Service
    }
    -> Url
    -> Cmd msg
pushUrl ({ device, key } as model) url =
    url
        |> Parser.parse (match model)
        |> Maybe.map
            (\route ->
                case route of
                    Page page ->
                        page |> Page.toUrl

                    Modal modal ->
                        modal |> Modal.toUrl

                    Service service ->
                        service |> Service.toUrl

                    Aside ->
                        if device |> Device.isPhoneOrTablet then
                            Aside.toUrl

                        else
                            model.service
                                |> Maybe.map Service.toUrl
                                |> Maybe.withDefault
                                    (model.modal
                                        |> Maybe.map Modal.toUrl
                                        |> Maybe.withDefault (model.page |> Page.toUrl)
                                    )

                    Exit ->
                        model.service
                            |> Maybe.map
                                (\_ ->
                                    model.modal
                                        |> Maybe.map Modal.toUrl
                                        |> Maybe.withDefault (model.page |> Page.toUrl)
                                )
                            |> Maybe.withDefault
                                (model.modal
                                    |> Maybe.map (\_ -> model.page |> Page.toUrl)
                                    |> Maybe.withDefault Router.toAllMarket
                                )
            )
        |> Maybe.withDefault Router.toAllMarket
        |> Navigation.pushUrl key


fromUrl :
    { model
        | device : Device
        , time : Posix
        , tokens : Tokens
        , pools : Pools
        , user : Maybe { user | chain : Chain, positions : Remote Positions }
    }
    -> Url
    -> Maybe Route
fromUrl model url =
    url |> Parser.parse (match model)


match :
    { model
        | device : Device
        , time : Posix
        , tokens : Tokens
        , pools : Pools
        , user : Maybe { user | chain : Chain, positions : Remote Positions }
    }
    -> Parser (Route -> a) a
match model =
    Parser.fragment <| fromFragment model


fromFragment :
    { model
        | device : Device
        , time : Posix
        , tokens : Tokens
        , pools : Pools
        , user : Maybe { user | chain : Chain, positions : Remote Positions }
    }
    -> Maybe String
    -> Route
fromFragment model maybeString =
    maybeString
        |> Maybe.withDefault ""
        |> (\string ->
                string
                    |> Page.fromFragment model
                    |> Maybe.map Page
                    |> Maybe.withDefault
                        (string
                            |> Modal.fromFragment model
                            |> Maybe.map Modal
                            |> Maybe.withDefault
                                (string
                                    |> Service.fromFragment model
                                    |> Maybe.map Service
                                    |> Maybe.withDefault
                                        (string
                                            |> Aside.fromFragment model
                                            |> Maybe.map (\_ -> Aside)
                                            |> Maybe.withDefault Exit
                                        )
                                )
                        )
           )
