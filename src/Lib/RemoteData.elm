module Lib.RemoteData exposing (RemoteData(..), map, withDefault)


type RemoteData a
    = Loading
    | Success a
    | Failure


withDefault : a -> RemoteData a -> a
withDefault default remoteData =
    case remoteData of
        Loading ->
            default

        Success a ->
            a

        Failure ->
            default


map : (a -> b) -> RemoteData a -> RemoteData b
map f remoteData =
    case remoteData of
        Loading ->
            Loading

        Success a ->
            Success (f a)

        Failure ->
            Failure
