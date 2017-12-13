module DjangoChannels
    exposing
        ( streamDemultiplexer
        )

{-|

# Utilities
@docs streamDemultiplexer

-}

-- Core modules

import Json.Decode exposing (Decoder, string, field, list)


{-| Deal with streams from a WebsocketDemultiplexer.

    import DjangoChannels exposing (streamDemultiplexer)

    type Stream
        = Initial
        | Todo
        | NotFound

    stringToStream str =
        "initial" ->
            Initial

        "todo" ->
            Todo

        _ ->
            NotFound

    demultiplex data =
        case streamDemultiplexer stringToStream NotFound of
            Initial ->
                -- Handle the "initial" stream
            Todo ->
                -- Handle the "todo" stream
            NotFound ->
                -- Handle when an unknown stream occurs

-}
streamDemultiplexer : String -> (String -> streamType) -> streamType -> streamType
streamDemultiplexer data stringToStream notFoundStream =
    case Json.Decode.decodeString (field "stream" string) data of
        Ok stream ->
            stringToStream stream

        Err _ ->
            notFoundStream
