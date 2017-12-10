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


-- External modules

import Json.Decode.Pipeline exposing (decode, requiredAt)


{-| Deal with streams from a WebsocketDemultiplexer.
-}
streamDemultiplexer : String -> (String -> streamType) -> streamType -> streamType
streamDemultiplexer data stringToStream notFoundStream =
    case Json.Decode.decodeString (field "stream" string) data of
        Ok stream ->
            stringToStream stream

        Err _ ->
            notFoundStream
