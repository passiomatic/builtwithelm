module Data.Pager exposing
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
    = Pager (Config a) (State a)


type alias Config a =
    { perPage : Int
    , toSearchTerm : a -> String
    }


type alias State a =
    { data : List a
    , pageNumber : Int
    , query : String
    }


fromList : Int -> (a -> String) -> List a -> Pager a
fromList perPage toSearchTerm data =
    let
        config =
            Config (clamp 5 100 perPage) toSearchTerm

        state =
            State data 1 ""
    in
    Pager config state


withPerPage : Int -> Pager a -> Pager a
withPerPage perPage (Pager config state) =
    Pager { config | perPage = clamp 5 100 perPage } state


type alias Page a =
    { data : List a
    , pageNumber : Int
    , hasPrev : Bool
    , hasNext : Bool
    }


currentPage : Pager a -> Page a
currentPage (Pager config state) =
    let
        { filteredData, pageNumber, hasPrev, hasNext } =
            toCurrentPageState config state

        data =
            filteredData
                |> List.drop ((pageNumber - 1) * config.perPage)
                |> List.take config.perPage
    in
    Page data pageNumber hasPrev hasNext


searchFor : String -> Pager a -> Pager a
searchFor query (Pager config state) =
    Pager config { state | query = query }


goto : Int -> Pager a -> Pager a
goto pageNumber (Pager config state) =
    Pager config { state | pageNumber = max 1 pageNumber }


prev : Pager a -> Pager a
prev ((Pager config state) as pager) =
    let
        { pageNumber, hasPrev } =
            toCurrentPageState config state
    in
    if hasPrev then
        Pager config { state | pageNumber = pageNumber - 1 }

    else
        pager


next : Pager a -> Pager a
next ((Pager config state) as pager) =
    let
        { pageNumber, hasNext } =
            toCurrentPageState config state
    in
    if hasNext then
        Pager config { state | pageNumber = pageNumber + 1 }

    else
        pager



-- HELPERS


type alias CurrentPageState a =
    { filteredData : List a
    , totalPages : Int
    , pageNumber : Int
    , hasPrev : Bool
    , hasNext : Bool
    }


toCurrentPageState : Config a -> State a -> CurrentPageState a
toCurrentPageState config state =
    let
        filteredData =
            List.filter isSimilar state.data

        isSimilar item =
            String.contains
                (String.toLower state.query)
                (String.toLower (config.toSearchTerm item))

        total =
            List.length filteredData

        totalPages =
            toTotalPages total config.perPage

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
    CurrentPageState filteredData totalPages pageNumber hasPrev hasNext


toTotalPages : Int -> Int -> Int
toTotalPages total perPage =
    let
        n =
            total // perPage
    in
    if modBy perPage total == 0 then
        n

    else
        n + 1
