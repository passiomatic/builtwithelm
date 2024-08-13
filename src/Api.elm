module Api exposing (fetchProjects)

import Data.Project exposing (Project)
import Http
import Json.Decode as JD


fetchProjects : (Result Http.Error (List Project) -> msg) -> Cmd msg
fetchProjects toMsg =
    Http.get
        { url = "/data/projects.json"
        , expect = Http.expectJson toMsg projectsDecoder
        }


projectsDecoder : JD.Decoder (List Project)
projectsDecoder =
    JD.list projectDecoder


projectDecoder : JD.Decoder Project
projectDecoder =
    JD.map5 Project
        (JD.field "name" JD.string)
        (JD.field "description" JD.string)
        (JD.field "primaryUrl" JD.string)
        (JD.field "previewImageUrl" JD.string)
        (JD.field "repositoryUrl" (JD.nullable JD.string))
