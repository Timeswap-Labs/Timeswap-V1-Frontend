module Route exposing (Route(..), fromUrl, pushUrl, toUrl)

import Browser.Navigation as Navigation exposing (Key)
import Data.Chain exposing (Chain(..))
import Modal exposing (Modal)
import Page exposing (Page)
import Service exposing (Service)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Route
    = Page Page
    | Modal Modal
    | Service Service
    | Exit


pushUrl :
    { model
        | key : Key
        , user : Maybe { user | chain : Chain }
        , page : Page
        , modal : Maybe Modal
        , service : Maybe Service
    }
    -> Url
    -> Cmd msg
pushUrl ({ key } as model) url =
    url
        |> Parser.parse (match model)
        |> Maybe.map
            (\route ->
                case route of
                    Page page ->
                        page |> Page.toFragment

                    Modal modal ->
                        modal |> Modal.toFragment

                    Service service ->
                        service |> Service.toFragment

                    Exit ->
                        model.service
                            |> Maybe.map
                                (\_ ->
                                    model.modal
                                        |> Maybe.map (\modal -> modal |> Modal.toFragment)
                                        |> Maybe.withDefault (model.page |> Page.toFragment)
                                )
                            |> Maybe.withDefault
                                (model.modal
                                    |> Maybe.map (\_ -> model.page |> Page.toFragment)
                                    |> Maybe.withDefault "market"
                                )
            )
        |> Maybe.withDefault "market"
        |> (++) "/#"
        |> Navigation.pushUrl key


fromUrl : { model | user : Maybe { user | chain : Chain } } -> Url -> Maybe Route
fromUrl model url =
    url |> Parser.parse (match model)


toUrl : Route -> String
toUrl route =
    (case route of
        Page page ->
            page |> Page.toFragment

        Modal modal ->
            modal |> Modal.toFragment

        Service service ->
            service |> Service.toFragment

        Exit ->
            ""
    )
        |> (++) "#"


match : { model | user : Maybe { user | chain : Chain } } -> Parser (Route -> a) a
match model =
    Parser.fragment <| fragment model


fragment : { model | user : Maybe { user | chain : Chain } } -> Maybe String -> Route
fragment model maybeString =
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
                                    |> Maybe.withDefault Exit
                                )
                        )
           )
