module Lib.Maybe exposing (catMaybes)


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.foldr
        (\maybeA list ->
            case maybeA of
                Just a ->
                    a :: list

                Nothing ->
                    list
        )
        []
