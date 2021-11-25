module Services.Swap.GameToken exposing (GameToken(..), decoder, encode, toApiTokenId, toERC20, toToken)

import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Token as Token exposing (Token)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type GameToken
    = Uniswap
    | Balancer
    | Shiba


encode : GameToken -> Value
encode gameToken =
    (case gameToken of
        Uniswap ->
            "uniswap"

        Balancer ->
            "balancer"

        Shiba ->
            "shiba"
    )
        |> Encode.string


decoder : Decoder GameToken
decoder =
    Decode.string
        |> Decode.andThen
            (\tokenStr ->
                case tokenStr of
                    "uniswap" ->
                        Uniswap |> Decode.succeed

                    "balancer" ->
                        Balancer |> Decode.succeed

                    "shiba" ->
                        Shiba |> Decode.succeed

                    _ ->
                        Decode.fail "Not a game token"
            )


toApiTokenId : GameToken -> String
toApiTokenId gameToken =
    case gameToken of
        Uniswap ->
            "uniswap"

        Balancer ->
            "balancer"

        Shiba ->
            "shiba-inu"


toERC20 : GameToken -> ERC20
toERC20 gameToken =
    case gameToken of
        Uniswap ->
            ERC20.uniswap

        Balancer ->
            ERC20.balancer

        Shiba ->
            ERC20.shiba


toToken : GameToken -> Token
toToken gameToken =
    gameToken |> toERC20 |> Token.ERC20
