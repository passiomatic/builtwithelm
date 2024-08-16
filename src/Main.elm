module Main exposing (main)

import Browser as B
import Browser.Navigation as BN
import Data.Route as Route
import Page.Home
import Page.NotFound
import Url exposing (Url)


main : Program () Model Msg
main =
    B.application
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
    , key : BN.Key
    , page : Page
    }


type Page
    = Home Page.Home.Model
    | NotFound


init : () -> Url -> BN.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( page, pageCmd ) =
            urlToPage url key
    in
    ( { url = url
      , key = key
      , page = page
      }
    , pageCmd
    )


urlToPage : Url -> BN.Key -> ( Page, Cmd Msg )
urlToPage url key =
    case Route.fromUrl url of
        Just (Route.Home params) ->
            Page.Home.init
                { key = key
                , params = params
                , onChange = ChangedHomePage
                }
                |> Tuple.mapFirst Home

        Nothing ->
            ( NotFound, Cmd.none )



-- UPDATE


type Msg
    = ClickedLink B.UrlRequest
    | ChangedUrl Url
    | ChangedHomePage Page.Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                B.Internal url ->
                    ( model
                    , BN.pushUrl model.key (Url.toString url)
                    )

                B.External url ->
                    ( model
                    , BN.load url
                    )

        ChangedUrl url ->
            case Route.fromUrl url of
                Just (Route.Home params) ->
                    case model.page of
                        Home pageModel ->
                            let
                                page =
                                    pageModel
                                        |> Page.Home.withParams params
                                        |> Home
                            in
                            ( { model | url = url, page = page }
                            , Cmd.none
                            )

                        _ ->
                            let
                                ( page, pageCmd ) =
                                    Page.Home.init
                                        { key = model.key
                                        , params = params
                                        , onChange = ChangedHomePage
                                        }
                                        |> Tuple.mapFirst Home
                            in
                            ( { model | url = url, page = page }
                            , pageCmd
                            )

                Nothing ->
                    ( { model | url = url, page = NotFound }
                    , Cmd.none
                    )

        ChangedHomePage pageMsg ->
            case model.page of
                Home pageModel ->
                    let
                        ( newPageModel, pageCmd ) =
                            Page.Home.update
                                { onChange = ChangedHomePage
                                }
                                pageMsg
                                pageModel
                    in
                    ( { model | page = Home newPageModel }
                    , pageCmd
                    )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> B.Document Msg
view model =
    { title = "Built with Elm"
    , body =
        [ case model.page of
            Home pageModel ->
                Page.Home.view
                    { onChange = ChangedHomePage
                    }
                    pageModel

            NotFound ->
                Page.NotFound.view
        ]
    }
