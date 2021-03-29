module Screen.Home exposing (Model, init, withParams, Msg, update, view)


import Api
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as E
import Html.Keyed
import Http
import Pager exposing (Pager)
import Project exposing (Project)
import RemoteData exposing (RemoteData(..))
import Route
import Task


-- MODEL


type alias Model =
  { key : Nav.Key
  , remoteData : RemoteData (Pager Project)
  , pageSize : Int
  , query : String
  , pageNumber : Int
  }


init : Nav.Key -> Route.HomeParams -> (Model, Cmd Msg)
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


update : Msg -> Model -> (Model, Cmd Msg)
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
          , Nav.pushUrl model.key <|
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
          , Nav.pushUrl model.key <|
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
          , Nav.pushUrl model.key <|
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
  Task.perform (always ScrolledToTop) (Dom.setViewport 0 0)


homeHref : String -> Int -> String
homeHref query pageNumber =
  Route.href (Route.Home { query = Just query, pageNumber = Just pageNumber })


currentPageNumber : RemoteData (Pager a) -> Int
currentPageNumber remoteData =
  remoteData
    |> RemoteData.map (Pager.currentPage >> .pageNumber)
    |> RemoteData.withDefault 1


-- VIEW


view : Model -> List (Html Msg)
view model =
  [ case model.remoteData of
      Loading ->
        div
          [ class "builtwithelm-Container" ]
          [ viewSidebar model.query
          , div
              [ class "builtwithelm-Content" ]
              [ div [ class "builtwithelm-ListContainer" ]
                  [ text "Loading..." ]
              ]
          ]

      Failure ->
        div
          [ class "builtwithelm-Container" ]
          [ viewSidebar model.query
          , div
              [ class "builtwithelm-Content" ]
              [ div [ class "builtwithelm-ListContainer" ]
                  [ text "Unable to load projects" ]
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
        div
          [ class "builtwithelm-Container" ]
          [ viewSidebar model.query
          , div
              [ class "builtwithelm-Content" ]
              [ Html.Keyed.node
                  "div"
                  [ class "builtwithelm-ListContainer" ]
                  (viewProjects page.data)
              , div
                  [ class "builtwithelm-Paging" ]
                  [ viewPageSizeSelect model.pageSize [ 5, 25, 50, 100 ]
                  , viewPageButton PressedPrev disablePrev "Newer"
                  , viewPageButton PressedNext disableNext "Older"
                  ]
              ]
          ]
  ]


viewSidebar : String -> Html Msg
viewSidebar query =
  div [ class "builtwithelm-Sidebar" ]
    [ div [ class "builtwithelm-SidebarHeader" ]
        [ div
            [ class "builtwithelm-SidebarLogoContainer" ]
            [ a [ href "/" ]
                [ img [ src "images/logo.svg", class "builtwithelm-Logo" ] [] ]
            ]
        , h1 []
            [ a
                [ href "/"
                , class "builtwithelm-BuiltWithLink"
                ]
                [ span
                    [ class "builtwithelm-BuiltWithText" ]
                    [ text "builtwith" ]
                , span [] [ text "elm" ]
                ]
            ]
        ]
    , div [ class "builtwithelm-SearchContainer" ]
        [ input
            [ type_ "text"
            , placeholder "Search"
            , value query
            , autofocus True
            , E.onInput EnteredQuery
            , class "builtwithelm-SearchInput"
            ]
            []
        ]
    , div [ class "builtwithelm-Links"]
        [ div [] [ a [ href "https://github.com/dwayne/builtwithelm#submissions" ] [ text "Submit a Project" ] ]
        , div [] [ a [ href "https://github.com/dwayne/builtwithelm/tree/master/docs/learn-elm.md" ] [ text "Learn Elm" ] ]
        , div [ class "builtwithelm-LastLink" ] [ a [ href "https://github.com/dwayne/builtwithelm#history" ] [ text "About" ] ]
        ]
    , div
        [ class "builtwithelm-BuiltBy" ]
        [ span [] [ text "Built by " ]
        , a [ href "https://github.com/dwayne", target "_blank" ] [ text "Dwayne Crooks" ]
        , span [] [ text " and " ]
        , a [ href "https://github.com/dwayne/builtwithelm/graphs/contributors", target "_blank" ] [ text "the amazing Elm community." ]
        ]
    ]


viewProjects : List Project -> List (String, Html msg)
viewProjects projects =
  List.map (\p -> (p.primaryUrl, viewProject p)) projects


viewProject : Project -> Html msg
viewProject project =
  div
    [ class "builtwithelm-Project" ]
    [ div
        [ class "builtwithelm-ProjectHeader" ]
        [ a
            [ href project.primaryUrl
            , target "_blank"
            , class "builtwithelm-Link"
            ]
            [ h2 []
                [ text project.name ]
            ]
        , viewOpenSourceLink project
        ]
    , p [] [ text project.description ]
    , div
        [ class "builtwithelm-ProjectScreenshotShell" ]
        [ img
            [ src project.previewImageUrl
            , class "builtwithelm-ProjectImage"
            ]
            []
        ]
    ]


viewOpenSourceLink : Project -> Html msg
viewOpenSourceLink project =
  case project.repositoryUrl of
    Just url ->
      a
        [ href url
        , target "_blank"
        , class "builtwithelm-Link"
        ]
        [ img
            [ src "images/github.svg"
            , class "builtwithelm-GithubLogo"
            ]
            []
        ]

    Nothing ->
      span [] []


viewPageSizeSelect : Int -> List Int -> Html Msg
viewPageSizeSelect current options =
  let
    toOption i =
      option [ value <| String.fromInt i ] [ text <| String.fromInt i ]
  in
  div [ class "builtwithelm-Dropdown" ]
      [ label [] [ text "Page size" ]
      , select
          [ value <| String.fromInt current
          , E.onInput ChangedPageSize
          ]
          (List.map toOption options)
      ]


viewPageButton : Msg -> Bool -> String -> Html Msg
viewPageButton onPress isDisabled label =
  if isDisabled then
    button
      [ disabled True
      , class "builtwithelm-Button"
      ]
      [ text label ]
  else
    button
      [ E.onClick onPress
      , class "builtwithelm-Button"
      ]
      [ text label ]
