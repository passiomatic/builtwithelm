module Route exposing (HomeParams, Route(..), fromUrl, href)

import Url
import Url.Builder
import Url.Parser exposing (Parser, map, oneOf, parse, query)
import Url.Parser.Query as Query


type Route
    = Home HomeParams


type alias HomeParams =
    { query : Maybe String
    , pageNumber : Maybe Int
    }


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    parse parser url


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home (query homeParamsParser)
        ]


homeParamsParser : Query.Parser HomeParams
homeParamsParser =
    Query.map2
        HomeParams
        (Query.string "q")
        (Query.int "page")


href : Route -> String
href route =
    case route of
        Home homeParams ->
            let
                toQ =
                    homeParams.query
                        |> Maybe.andThen
                            (\value ->
                                if String.isEmpty value then
                                    Nothing

                                else
                                    Just value
                            )
                        |> Maybe.map (Url.Builder.string "q")

                toPage =
                    homeParams.pageNumber
                        |> Maybe.andThen
                            (\value ->
                                if value <= 1 then
                                    Nothing

                                else
                                    Just value
                            )
                        |> Maybe.map (Url.Builder.int "page")

                queryParams =
                    compact [ toQ, toPage ]
            in
            Url.Builder.absolute [] queryParams


compact : List (Maybe a) -> List a
compact list =
    case list of
        [] ->
            []

        x :: xs ->
            case x of
                Nothing ->
                    compact xs

                Just a ->
                    a :: compact xs
