module Project exposing (Project)


type alias Project =
    { name : String
    , description : String
    , primaryUrl : String
    , previewImageUrl : String
    , repositoryUrl : Maybe String
    }
