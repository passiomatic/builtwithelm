module Pager exposing
    ( Page
    , Pager
    , currentPage
    , fromList
    , goto
    , next
    , prev
    , searchFor
    , withPerPage
    )


type Pager a
    = Pager (Settings a) (State a)


type alias Settings a =
    { perPage : Int
    , toSearchTerm : a -> String
    }


type alias State a =
    { data : List a
    , pageNumber : Int
    , query : String
    }


type alias Page a =
    { data : List a
    , pageNumber : Int
    , hasPrev : Bool
    , hasNext : Bool
    }


fromList : Int -> (a -> String) -> List a -> Pager a
fromList perPage toSearchTerm data =
    let
        settings =
            Settings (clamp 1 100 perPage) toSearchTerm

        state =
            State data 1 ""
    in
    Pager settings state


withPerPage : Int -> Pager a -> Pager a
withPerPage perPage (Pager settings state) =
    Pager { settings | perPage = clamp 1 100 perPage } state


currentPage : Pager a -> Page a
currentPage (Pager settings state) =
    let
        { filteredData, pageNumber, hasPrev, hasNext } =
            derivedState settings state

        data =
            filteredData
                |> List.drop ((pageNumber - 1) * settings.perPage)
                |> List.take settings.perPage
    in
    Page data pageNumber hasPrev hasNext


searchFor : String -> Pager a -> Pager a
searchFor query (Pager settings state) =
    Pager settings { state | query = query }


goto : Int -> Pager a -> Pager a
goto pageNumber (Pager settings state) =
    Pager settings { state | pageNumber = max 1 pageNumber }


prev : Pager a -> Pager a
prev ((Pager settings state) as pager) =
    let
        { pageNumber, hasPrev } =
            derivedState settings state
    in
    if hasPrev then
        Pager settings { state | pageNumber = pageNumber - 1 }

    else
        pager


next : Pager a -> Pager a
next ((Pager settings state) as pager) =
    let
        { pageNumber, hasNext } =
            derivedState settings state
    in
    if hasNext then
        Pager settings { state | pageNumber = pageNumber + 1 }

    else
        pager



-- HELPERS


type alias DerivedState a =
    { filteredData : List a
    , totalPages : Int
    , pageNumber : Int
    , hasPrev : Bool
    , hasNext : Bool
    }


derivedState : Settings a -> State a -> DerivedState a
derivedState settings state =
    let
        isSimilar elem =
            String.contains
                (String.toLower state.query)
                (String.toLower (settings.toSearchTerm elem))

        filteredData =
            List.filter isSimilar state.data

        total =
            List.length filteredData

        totalPages =
            numberOfPages total settings.perPage

        pageNumber =
            if total == 0 then
                1

            else
                clamp 1 totalPages state.pageNumber

        hasPrev =
            pageNumber > 1

        hasNext =
            pageNumber < totalPages
    in
    DerivedState filteredData totalPages pageNumber hasPrev hasNext


numberOfPages : Int -> Int -> Int
numberOfPages totalPages perPage =
    let
        n =
            totalPages // perPage
    in
    if modBy perPage totalPages == 0 then
        n

    else
        n + 1
