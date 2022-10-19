module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
import Route
import Screen.Home
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



-- MODEL


type alias Model =
    { url : Url
    , key : Nav.Key
    , screen : Screen
    }


type Screen
    = Home Screen.Home.Model
    | NotFound


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    fromUrl url key
        |> Tuple.mapFirst (Model url key)


fromUrl : Url -> Nav.Key -> ( Screen, Cmd Msg )
fromUrl url key =
    case Route.fromUrl url of
        Just (Route.Home params) ->
            Screen.Home.init key params
                |> Tuple.mapBoth Home (Cmd.map HomeMsg)

        Nothing ->
            ( NotFound
            , Cmd.none
            )



-- UPDATE


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | HomeMsg Screen.Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ChangedUrl url ->
            case Route.fromUrl url of
                Just (Route.Home params) ->
                    case model.screen of
                        Home homeModel ->
                            let
                                screen =
                                    homeModel
                                        |> Screen.Home.withParams params
                                        |> Home
                            in
                            ( { model | url = url, screen = screen }
                            , Cmd.none
                            )

                        _ ->
                            let
                                ( screen, _ ) =
                                    Screen.Home.init model.key params
                                        |> Tuple.mapBoth Home (Cmd.map HomeMsg)
                            in
                            ( { model | url = url, screen = screen }
                            , Cmd.none
                            )

                Nothing ->
                    ( { model | url = url, screen = NotFound }
                    , Cmd.none
                    )

        HomeMsg homeMsg ->
            case model.screen of
                Home homeModel ->
                    let
                        ( screen, cmd ) =
                            Screen.Home.update homeMsg homeModel
                                |> Tuple.mapBoth Home (Cmd.map HomeMsg)
                    in
                    ( { model | screen = screen }, cmd )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Built with Elm"
    , body =
        case model.screen of
            Home homeModel ->
                Screen.Home.view homeModel
                    |> List.map (Html.map HomeMsg)

            NotFound ->
                [ Html.text "Not found" ]
    }
