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
import Lib.RemoteData as RemoteData exposing (RemoteData)



-- MODEL


type alias Model =
    { key : BN.Key
    , remoteData : RemoteData (Pager Project)
    , pageSize : Int
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
      , pageSize = 5
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


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotProjects result ->
            ( case result of
                Ok projects ->
                    let
                        pager =
                            Pager.fromList model.pageSize .name projects
                                |> Pager.searchFor model.query
                                |> Pager.goto model.pageNumber
                    in
                    { model | remoteData = RemoteData.Success pager }

                Err _ ->
                    { model | remoteData = RemoteData.Failure }
            , Cmd.none
            )



-- VIEW


view : Model -> H.Html msg
view model =
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
                [ viewSidebar
                , viewContent page.data
                ]

        RemoteData.Failure ->
            --
            -- TODO: Improve the error view.
            --
            H.text "Sorry, we're unable to load the projects."


viewSidebar : H.Html msg
viewSidebar =
    H.div [ HA.class "builtwithelm__sidebar" ]
        [ viewHeading
        , viewSearch Nothing
        , viewFooter
        ]


viewContent : List Project -> H.Html msg
viewContent projects =
    H.div [ HA.class "builtwithelm__content" ]
        [ viewProjects projects
        , viewFooter
        ]


viewProjects : List Project -> H.Html msg
viewProjects projects =
    H.div [ HA.class "builtwithelm__projects" ] <|
        List.map viewProject projects
            ++ [ viewPager ]


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


viewSearch : Maybe (String -> msg) -> H.Html msg
viewSearch maybeOnInput =
    H.form [ HA.class "search" ]
        [ H.input
            [ HA.class "search__input"
            , HA.type_ "text"
            , HA.placeholder "Find an awesome Elm web app"
            , HA.autofocus True
            , case maybeOnInput of
                Just onInput ->
                    HE.onInput onInput

                Nothing ->
                    HA.disabled True
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


viewPager : H.Html msg
viewPager =
    H.div [ HA.class "pager" ]
        [ H.div [ HA.class "pager__setting" ]
            [ viewPageSize
            ]
        , H.div [ HA.class "pager__buttons" ]
            [ viewButton
                { text = "Newer"
                , maybeOnClick = Nothing
                }
            , viewButton
                { text = "Older"
                , maybeOnClick = Nothing
                }
            ]
        ]


viewPageSize : H.Html msg
viewPageSize =
    H.div [ HA.class "setting" ]
        [ H.label [ HA.for "page-size" ]
            [ H.text "Page size"
            ]
        , H.text " "
        , H.select [ HA.id "page-size" ]
            [ H.option [ HA.value "5" ] [ H.text "5" ]
            , H.option [ HA.value "25" ] [ H.text "25" ]
            , H.option [ HA.value "50" ] [ H.text "50" ]
            , H.option [ HA.value "100" ] [ H.text "100" ]
            ]
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
