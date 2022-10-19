module Api exposing (fetchProjects)

import Http
import Json.Decode as D exposing (Decoder)
import Project exposing (Project)


fetchProjects : (Result Http.Error (List Project) -> msg) -> Cmd msg
fetchProjects toMsg =
    Http.get
        { url = "/data/projects.json"
        , expect = Http.expectJson toMsg projectsDecoder
        }


projectsDecoder : Decoder (List Project)
projectsDecoder =
    D.list projectDecoder


projectDecoder : Decoder Project
projectDecoder =
    D.map5 Project
        (D.field "name" D.string)
        (D.field "description" D.string)
        (D.field "primaryUrl" D.string)
        (D.field "previewImageUrl" D.string)
        (D.field "repositoryUrl" (D.maybe D.string))
