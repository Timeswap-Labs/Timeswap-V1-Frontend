module Utility.Scroll exposing (Visibility, toPositions, visibility)

import Browser.Dom as Dom
import Browser.Events
import SmoothScroll exposing (Config)
import Task


type alias Visibility =
    { headerGlass : Browser.Events.Visibility
    , scrollToPositions : Browser.Events.Visibility
    }


visibility :
    (Visibility -> msg)
    -> Cmd msg
visibility msg =
    Dom.getViewport
        |> Task.andThen
            (\receivedViewport ->
                Dom.getElement "positions"
                    |> Task.map Just
                    |> Task.onError (\_ -> Task.succeed Nothing)
                    |> Task.map2
                        (\{ viewport } positions ->
                            { headerGlass =
                                if viewport.y > 20 then
                                    Browser.Events.Visible

                                else
                                    Browser.Events.Hidden
                            , scrollToPositions =
                                positions
                                    |> Maybe.map
                                        (\{ element } ->
                                            if element.y + 80 > viewport.y + viewport.height then
                                                Browser.Events.Visible

                                            else
                                                Browser.Events.Hidden
                                        )
                                    |> Maybe.withDefault Browser.Events.Hidden
                            }
                        )
                        (Task.succeed receivedViewport)
            )
        |> Task.perform msg


toPositions : (() -> msg) -> Cmd msg
toPositions msg =
    "positions"
        |> SmoothScroll.scrollToWithOptions config
        |> Task.map (\_ -> ())
        |> Task.onError (\_ -> Task.succeed ())
        |> Task.perform msg


config : Config
config =
    SmoothScroll.defaultConfig
        |> (\defaultConfig ->
                { defaultConfig
                    | offset = 100
                    , speed = 30
                }
           )
