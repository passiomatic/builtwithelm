module Screen.Home exposing
    ( Model
    , Msg
    , init
    , update
    , view
    , withParams
    )

import Api
import Browser.Dom as BD
import Browser.Navigation as BN
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Http
import Pager exposing (Pager)
import Project exposing (Project)
import RemoteData exposing (RemoteData(..))
import Route
import Task



-- MODEL


type alias Model =
    { key : BN.Key
    , remoteData : RemoteData (Pager Project)
    , pageSize : Int
    , query : String
    , pageNumber : Int
    }


init : BN.Key -> Route.HomeParams -> ( Model, Cmd Msg )
init key homeParams =
    ( { key = key
      , remoteData = Loading
      , pageSize = 5
      , query = Maybe.withDefault "" homeParams.query
      , pageNumber = Maybe.withDefault 1 homeParams.pageNumber
      }
    , Api.fetchProjects GotProjects
    )


withParams : Route.HomeParams -> Model -> Model
withParams homeParams model =
    let
        query =
            Maybe.withDefault "" homeParams.query

        pageNumber =
            Maybe.withDefault 1 homeParams.pageNumber
    in
    { model
        | query = query
        , pageNumber = pageNumber
        , remoteData =
            RemoteData.map
                (Pager.searchFor query >> Pager.goto pageNumber)
                model.remoteData
    }



-- UPDATE


type Msg
    = PressedPrev
    | PressedNext
    | ScrolledToTop
    | EnteredQuery String
    | ChangedPageSize String
    | GotProjects (Result Http.Error (List Project))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedPrev ->
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

        PressedNext ->
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
            ( model
            , Cmd.none
            )

        EnteredQuery query ->
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

        ChangedPageSize pageSizeString ->
            ( let
                pageSize =
                    String.toInt pageSizeString
                        |> Maybe.withDefault 5

                remoteData =
                    RemoteData.map (Pager.withPerPage pageSize) model.remoteData
              in
              { model | remoteData = remoteData, pageSize = pageSize }
            , Cmd.none
            )

        GotProjects (Ok projects) ->
            ( { model
                | remoteData =
                    Pager.fromList model.pageSize .name projects
                        |> Pager.searchFor model.query
                        |> Pager.goto model.pageNumber
                        |> Success
              }
            , Cmd.none
            )

        GotProjects (Err _) ->
            ( { model | remoteData = Failure }
            , Cmd.none
            )


scrollToTop : Cmd Msg
scrollToTop =
    Task.perform (always ScrolledToTop) (BD.setViewport 0 0)


homeHref : String -> Int -> String
homeHref query pageNumber =
    Route.href (Route.Home { query = Just query, pageNumber = Just pageNumber })


currentPageNumber : RemoteData (Pager a) -> Int
currentPageNumber remoteData =
    remoteData
        |> RemoteData.map (Pager.currentPage >> .pageNumber)
        |> RemoteData.withDefault 1



-- VIEW


view : Model -> List (H.Html Msg)
view model =
    [ case model.remoteData of
        Loading ->
            H.div
                [ HA.class "builtwithelm-Container" ]
                [ viewSidebar model.query
                , H.div
                    [ HA.class "builtwithelm-Content" ]
                    [ H.div [ HA.class "builtwithelm-ListContainer" ]
                        [ H.text "Loading..." ]
                    ]
                ]

        Failure ->
            H.div
                [ HA.class "builtwithelm-Container" ]
                [ viewSidebar model.query
                , H.div
                    [ HA.class "builtwithelm-Content" ]
                    [ H.div [ HA.class "builtwithelm-ListContainer" ]
                        [ H.text "Unable to load projects" ]
                    ]
                ]

        Success pager ->
            let
                page =
                    Pager.currentPage pager

                disablePrev =
                    not page.hasPrev

                disableNext =
                    not page.hasNext
            in
            H.div
                [ HA.class "builtwithelm-Container" ]
                [ viewSidebar model.query
                , H.div
                    [ HA.class "builtwithelm-Content" ]
                    [ HK.node
                        "div"
                        [ HA.class "builtwithelm-ListContainer" ]
                        (viewProjects page.data)
                    , H.div
                        [ HA.class "builtwithelm-Paging" ]
                        [ viewPageSizeSelect model.pageSize [ 5, 25, 50, 100 ]
                        , viewPageButton PressedPrev disablePrev "Newer"
                        , viewPageButton PressedNext disableNext "Older"
                        ]
                    ]
                ]
    ]


viewSidebar : String -> H.Html Msg
viewSidebar query =
    H.div [ HA.class "builtwithelm-Sidebar" ]
        [ H.div [ HA.class "builtwithelm-SidebarHeader" ]
            [ H.div
                [ HA.class "builtwithelm-SidebarLogoContainer" ]
                [ H.a [ HA.href "/" ]
                    [ H.img [ HA.src "images/logo.svg", HA.class "builtwithelm-Logo" ] [] ]
                ]
            , H.h1 []
                [ H.a
                    [ HA.href "/"
                    , HA.class "builtwithelm-BuiltWithLink"
                    ]
                    [ H.span
                        [ HA.class "builtwithelm-BuiltWithText" ]
                        [ H.text "builtwith" ]
                    , H.span [] [ H.text "elm" ]
                    ]
                ]
            ]
        , H.div [ HA.class "builtwithelm-SearchContainer" ]
            [ H.input
                [ HA.type_ "text"
                , HA.placeholder "Search"
                , HA.value query
                , HA.autofocus True
                , HE.onInput EnteredQuery
                , HA.class "builtwithelm-SearchInput"
                ]
                []
            ]
        , H.div [ HA.class "builtwithelm-Links" ]
            [ H.div [] [ H.a [ HA.href "https://github.com/dwayne/builtwithelm#submissions" ] [ H.text "Submit a Project" ] ]
            , H.div [] [ H.a [ HA.href "https://github.com/dwayne/builtwithelm/tree/master/docs/learn-elm.md" ] [ H.text "Learn Elm" ] ]
            , H.div [ HA.class "builtwithelm-LastLink" ] [ H.a [ HA.href "https://github.com/dwayne/builtwithelm#history" ] [ H.text "About" ] ]
            ]
        , H.div
            [ HA.class "builtwithelm-BuiltBy" ]
            [ H.span [] [ H.text "Built by " ]
            , H.a [ HA.href "https://github.com/dwayne", HA.target "_blank" ] [ H.text "Dwayne Crooks" ]
            , H.span [] [ H.text " and " ]
            , H.a [ HA.href "https://github.com/dwayne/builtwithelm/graphs/contributors", HA.target "_blank" ] [ H.text "the amazing Elm community." ]
            ]
        ]


viewProjects : List Project -> List ( String, H.Html msg )
viewProjects projects =
    List.map (\p -> ( p.primaryUrl, viewProject p )) projects


viewProject : Project -> H.Html msg
viewProject project =
    H.div
        [ HA.class "builtwithelm-Project" ]
        [ H.div
            [ HA.class "builtwithelm-ProjectHeader" ]
            [ H.a
                [ HA.href project.primaryUrl
                , HA.target "_blank"
                , HA.class "builtwithelm-Link"
                ]
                [ H.h2 []
                    [ H.text project.name ]
                ]
            , viewOpenSourceLink project
            ]
        , H.p [] [ H.text project.description ]
        , H.div
            [ HA.class "builtwithelm-ProjectScreenshotShell" ]
            [ H.img
                [ HA.src project.previewImageUrl
                , HA.class "builtwithelm-ProjectImage"
                ]
                []
            ]
        ]


viewOpenSourceLink : Project -> H.Html msg
viewOpenSourceLink project =
    case project.repositoryUrl of
        Just url ->
            H.a
                [ HA.href url
                , HA.target "_blank"
                , HA.class "builtwithelm-Link"
                ]
                [ H.img
                    [ HA.src "images/github.svg"
                    , HA.class "builtwithelm-GithubLogo"
                    ]
                    []
                ]

        Nothing ->
            H.span [] []


viewPageSizeSelect : Int -> List Int -> H.Html Msg
viewPageSizeSelect current options =
    let
        toOption i =
            H.option [ HA.value <| String.fromInt i ] [ H.text <| String.fromInt i ]
    in
    H.div [ HA.class "builtwithelm-Dropdown" ]
        [ H.label [] [ H.text "Page size" ]
        , H.select
            [ HA.value <| String.fromInt current
            , HE.onInput ChangedPageSize
            ]
            (List.map toOption options)
        ]


viewPageButton : Msg -> Bool -> String -> H.Html Msg
viewPageButton onPress isDisabled label =
    if isDisabled then
        H.button
            [ HA.disabled True
            , HA.class "builtwithelm-Button"
            ]
            [ H.text label ]

    else
        H.button
            [ HE.onClick onPress
            , HA.class "builtwithelm-Button"
            ]
            [ H.text label ]
