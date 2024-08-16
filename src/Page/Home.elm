module Page.Home exposing
    ( InitOptions
    , Model
    , Msg
    , init
    , update
    , view
    , withParams
    )

import Api
import Browser.Navigation as BN
import Data.Pager as Pager exposing (Pager)
import Data.Project exposing (Project)
import Data.Route as Route
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Lib.Browser.Dom as BD
import Lib.RemoteData as RemoteData exposing (RemoteData)



-- MODEL


type alias Model =
    { key : BN.Key
    , remoteData : RemoteData (Pager Project)
    , perPage : Int
    , query : String
    , pageNumber : Int
    }


type alias InitOptions msg =
    { key : BN.Key
    , params : Route.HomeParams
    , onChange : Msg -> msg
    }


init : InitOptions msg -> ( Model, Cmd msg )
init { key, params, onChange } =
    ( { key = key
      , remoteData = RemoteData.Loading
      , perPage = 5
      , query = Maybe.withDefault "" params.query
      , pageNumber = Maybe.withDefault 1 params.pageNumber
      }
    , Api.fetchProjects GotProjects
        |> Cmd.map onChange
    )


withParams : Route.HomeParams -> Model -> Model
withParams params model =
    let
        query =
            Maybe.withDefault "" params.query

        pageNumber =
            Maybe.withDefault 1 params.pageNumber
    in
    { model
        | remoteData =
            RemoteData.map
                (Pager.searchFor query >> Pager.goto pageNumber)
                model.remoteData
        , query = query
        , pageNumber = pageNumber
    }



-- UPDATE


type Msg
    = GotProjects (Result Http.Error (List Project))
    | ClickedPrev
    | ClickedNext
    | ScrolledToTop
    | InputPerPage String
    | InputQuery String


type alias UpdateOptions msg =
    { onChange : Msg -> msg
    }


update : UpdateOptions msg -> Msg -> Model -> ( Model, Cmd msg )
update { onChange } msg model =
    updateHelper msg model
        |> Tuple.mapSecond (Cmd.map onChange)


updateHelper : Msg -> Model -> ( Model, Cmd Msg )
updateHelper msg model =
    case msg of
        GotProjects result ->
            ( case result of
                Ok projects ->
                    let
                        pager =
                            Pager.fromList model.perPage .name projects
                                |> Pager.searchFor model.query
                                |> Pager.goto model.pageNumber
                    in
                    { model | remoteData = RemoteData.Success pager }

                Err _ ->
                    { model | remoteData = RemoteData.Failure }
            , Cmd.none
            )

        ClickedPrev ->
            let
                remoteData =
                    RemoteData.map Pager.prev model.remoteData
            in
            ( { model | remoteData = remoteData }
            , Cmd.batch
                [ scrollToTop
                , BN.pushUrl model.key <|
                    homeHref model.query (currentPageNumber remoteData)
                ]
            )

        ClickedNext ->
            let
                remoteData =
                    RemoteData.map Pager.next model.remoteData
            in
            ( { model | remoteData = remoteData }
            , Cmd.batch
                [ scrollToTop
                , BN.pushUrl model.key <|
                    homeHref model.query (currentPageNumber remoteData)
                ]
            )

        ScrolledToTop ->
            ( model, Cmd.none )

        InputPerPage perPageAsString ->
            ( let
                perPage =
                    String.toInt perPageAsString
                        |> Maybe.withDefault 5

                remoteData =
                    RemoteData.map (Pager.withPerPage perPage) model.remoteData
              in
              { model | remoteData = remoteData, perPage = perPage }
            , scrollToTop
            )

        InputQuery query ->
            let
                remoteData =
                    RemoteData.map (Pager.searchFor query) model.remoteData
            in
            ( { model | remoteData = remoteData, query = query }
            , Cmd.batch
                [ scrollToTop
                , BN.pushUrl model.key <|
                    homeHref query (currentPageNumber remoteData)
                ]
            )


scrollToTop : Cmd Msg
scrollToTop =
    BD.scrollToTop ScrolledToTop


homeHref : String -> Int -> String
homeHref query pageNumber =
    Route.toString <|
        Route.Home
            { query = Just query
            , pageNumber = Just pageNumber
            }


currentPageNumber : RemoteData (Pager a) -> Int
currentPageNumber =
    RemoteData.map (Pager.currentPage >> .pageNumber)
        >> RemoteData.withDefault 1



-- VIEW


type alias ViewOptions msg =
    { onChange : Msg -> msg
    }


view : ViewOptions msg -> Model -> H.Html msg
view { onChange } model =
    case model.remoteData of
        RemoteData.Loading ->
            --
            -- TODO: Improve the loading view.
            --
            H.text "Loading..."

        RemoteData.Success pager ->
            let
                page =
                    Pager.currentPage pager
            in
            H.div [ HA.class "builtwithelm" ]
                [ viewSidebar InputQuery
                , viewContent
                    { projects = page.data
                    , current = model.perPage
                    , values = [ 5, 25, 50, 100 ]
                    , onInputPerPage = InputPerPage
                    , maybeOnPrev =
                        if page.hasPrev then
                            Just ClickedPrev

                        else
                            Nothing
                    , maybeOnNext =
                        if page.hasNext then
                            Just ClickedNext

                        else
                            Nothing
                    }
                ]
                |> H.map onChange

        RemoteData.Failure ->
            --
            -- TODO: Improve the error view.
            --
            H.text "Sorry, we're unable to load the projects."


viewSidebar : (String -> msg) -> H.Html msg
viewSidebar onInputQuery =
    H.div [ HA.class "builtwithelm__sidebar" ]
        [ viewHeading
        , viewSearch onInputQuery
        , viewFooter
        ]


viewContent : ViewProjectsOptions msg -> H.Html msg
viewContent projectsOptions =
    H.div [ HA.class "builtwithelm__content" ]
        [ viewProjects projectsOptions
        , viewFooter
        ]


type alias ViewProjectsOptions msg =
    { projects : List Project
    , current : Int
    , values : List Int
    , onInputPerPage : String -> msg
    , maybeOnPrev : Maybe msg
    , maybeOnNext : Maybe msg
    }


viewProjects : ViewProjectsOptions msg -> H.Html msg
viewProjects { projects, current, values, onInputPerPage, maybeOnPrev, maybeOnNext } =
    if List.isEmpty projects then
        H.div [ HA.class "builtwithelm__projects" ] []

    else
        H.div [ HA.class "builtwithelm__projects" ] <|
            List.map viewProject projects
                ++ [ viewPager
                        { current = current
                        , values = values
                        , onInputPerPage = onInputPerPage
                        , maybeOnPrev = maybeOnPrev
                        , maybeOnNext = maybeOnNext
                        }
                   ]


viewFooter : H.Html msg
viewFooter =
    H.footer [ HA.class "builtwithelm__footer" ]
        [ viewLinks
        , viewAttribution
        ]


viewHeading : H.Html msg
viewHeading =
    H.header [ HA.class "heading" ]
        [ H.a
            [ HA.class "heading__link"
            , HA.href <| Route.toString (Route.Home { query = Nothing, pageNumber = Nothing })
            ]
            [ H.img
                [ HA.class "heading__logo"
                , HA.src "/images/logo.svg"
                , HA.alt "The Elm tangram wrapped in a browser frame"
                ]
                []
            , H.h1 [ HA.class "heading__title" ]
                [ H.span [ HA.class "heading__prefix" ]
                    [ H.text "builtwith" ]
                , H.span [ HA.class "heading__suffix" ]
                    [ H.text "elm" ]
                ]
            ]
        ]


viewSearch : (String -> msg) -> H.Html msg
viewSearch onInput =
    H.form [ HA.class "search" ]
        [ H.input
            [ HA.class "search__input"
            , HA.type_ "text"
            , HA.placeholder "Find an awesome Elm web app"
            , HA.autofocus True
            , HE.onInput onInput
            ]
            []
        ]


viewProject : Project -> H.Html msg
viewProject project =
    H.div [ HA.class "project" ]
        [ H.header [ HA.class "project__header" ]
            [ H.h2 [ HA.class "project__title" ]
                [ viewExternalLink
                    { href = project.primaryUrl
                    , content = Text project.name
                    }
                ]
            , case project.repositoryUrl of
                Just url ->
                    viewExternalLink
                        { href = url
                        , content =
                            Custom
                                [ H.img
                                    [ HA.class "project__github-logo"
                                    , HA.src "/images/github.svg"
                                    , HA.alt "GitHub logo"
                                    ]
                                    []
                                ]
                        }

                Nothing ->
                    H.text ""
            ]
        , H.p [ HA.class "project__description" ]
            [ H.text project.description ]
        , H.div [ HA.class "project__screenshot" ]
            [ H.img [ HA.src project.previewImageUrl ] []
            ]
        ]


viewLinks : H.Html msg
viewLinks =
    H.ul [ HA.class "links" ]
        [ H.li []
            [ viewExternalLink
                { href = "https://github.com/dwayne/builtwithelm#submissions"
                , content = Text "Submit a Project"
                }
            ]
        , H.li []
            [ viewExternalLink
                { href = "https://github.com/dwayne/builtwithelm/tree/master/docs/learn-elm.md"
                , content = Text "Learn Elm"
                }
            ]
        , H.li []
            [ viewExternalLink
                { href = "https://github.com/dwayne/builtwithelm#history"
                , content = Text "About"
                }
            ]
        ]


viewPager :
    { current : Int
    , values : List Int
    , onInputPerPage : String -> msg
    , maybeOnPrev : Maybe msg
    , maybeOnNext : Maybe msg
    }
    -> H.Html msg
viewPager { current, values, onInputPerPage, maybeOnPrev, maybeOnNext } =
    H.div [ HA.class "pager" ]
        [ H.div [ HA.class "pager__setting" ]
            [ viewPerPage
                { current = current
                , values = values
                , onInput = onInputPerPage
                }
            ]
        , H.div [ HA.class "pager__buttons" ]
            [ viewButton
                { text = "Newer"
                , maybeOnClick = maybeOnPrev
                }
            , viewButton
                { text = "Older"
                , maybeOnClick = maybeOnNext
                }
            ]
        ]


viewPerPage :
    { current : Int
    , values : List Int

    --
    -- TODO: Improve to PerPage -> msg.
    --
    , onInput : String -> msg
    }
    -> H.Html msg
viewPerPage { current, values, onInput } =
    let
        toOption value =
            H.option [ HA.value value ] [ H.text value ]

        viewOptions =
            List.map (toOption << String.fromInt) values
    in
    H.div [ HA.class "setting" ]
        [ H.label [ HA.for "per-page" ] [ H.text "Projects per page" ]
        , H.text " "
        , H.select
            [ HA.id "per-page"
            , HA.value <| String.fromInt current
            , HE.onInput onInput
            ]
            viewOptions
        ]


viewButton : { text : String, maybeOnClick : Maybe msg } -> H.Html msg
viewButton { text, maybeOnClick } =
    H.button
        [ HA.class "button"
        , HA.type_ "button"
        , case maybeOnClick of
            Just onClick ->
                HE.onClick onClick

            Nothing ->
                HA.disabled True
        ]
        [ H.text text
        ]


viewAttribution : H.Html msg
viewAttribution =
    H.p [ HA.class "attribution" ]
        [ H.text "Built by "
        , viewExternalLink
            { href = "https://github.com/dwayne"
            , content = Text "Dwayne Crooks"
            }
        , H.text " and "
        , viewExternalLink
            { href = "https://github.com/dwayne/builtwithelm/graphs/contributors"
            , content = Text "the amazing Elm community"
            }
        , H.text "."
        ]


type Content msg
    = Text String
    | Custom (List (H.Html msg))


viewExternalLink : { href : String, content : Content msg } -> H.Html msg
viewExternalLink { href, content } =
    H.a [ HA.href href, HA.target "_blank" ] <|
        case content of
            Text text ->
                [ H.text text ]

            Custom body ->
                body
