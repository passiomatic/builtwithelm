module Data.Route exposing (HomeParams, Route(..), fromUrl, toString)

import Lib.Maybe as Maybe
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UP
import Url.Parser.Query as Query


type Route
    = Home HomeParams


type alias HomeParams =
    { query : Maybe String
    , pageNumber : Maybe Int
    }


fromUrl : Url -> Maybe Route
fromUrl =
    UP.parse routeParser


routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map Home (UP.query homeParamsParser)
        ]


homeParamsParser : Query.Parser HomeParams
homeParamsParser =
    Query.map2 HomeParams
        (Query.string "q")
        (Query.int "page")


toString : Route -> String
toString route =
    case route of
        Home { query, pageNumber } ->
            let
                toQ =
                    query
                        |> Maybe.andThen
                            (\value ->
                                if String.isEmpty value then
                                    Nothing

                                else
                                    Just value
                            )
                        |> Maybe.map (UB.string "q")

                toPage =
                    pageNumber
                        |> Maybe.andThen
                            (\value ->
                                if value <= 1 then
                                    Nothing

                                else
                                    Just value
                            )
                        |> Maybe.map (UB.int "page")

                queryParams =
                    Maybe.catMaybes [ toQ, toPage ]
            in
            UB.absolute [] queryParams
