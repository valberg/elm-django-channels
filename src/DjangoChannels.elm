module DjangoChannels
    exposing
        ( ModelBindingPayload
        , modelBindingDecoder
        , streamDecoder
        )

{-|

Decoders and such to ease communication with django channels.

# Definition
@docs ModelBindingPayload

# Decoders
@docs modelBindingDecoder, streamDecoder

-}

-- Core modules

import Json.Decode exposing (Decoder, string, field)


-- External modules

import Json.Decode.Pipeline exposing (decode, requiredAt)


{-| ModelBindingPayload has the shape of what is sent by channels when a
    WebsocketBinding is triggered.

-}
type alias ModelBindingPayload dataType pkType =
    { model : String
    , data : dataType
    , pk : pkType
    , action : String
    }


{-| Decode data sent from a channels WebsocketBinding.

    import Json.Decode exposing (Decoder, string, bool)
    import Json.Decode.Pipeline exposing (decode, required)

    type alias TodoItem =
        { pk : String
        , description : String
        , isDone : Bool
        }

    postBindingDecoder : Decoder (ModelBindingPayload Post String)
    postBindingDecoder =
        let
            todoItemDecoder : Decoder TodoItem
            todoItemDecoder =
                decode TodoItem
                |> required "pk" string
                |> required "description" string
                |> required "is_done" bool
        in
            modelBindingDecoder todoItemDecoder string

-}
modelBindingDecoder : Decoder a -> Decoder b -> Decoder (ModelBindingPayload a b)
modelBindingDecoder dataDecoder pkDecoder =
    decode ModelBindingPayload
        |> requiredAt [ "payload", "model" ] string
        |> requiredAt [ "payload", "data" ] dataDecoder
        |> requiredAt [ "payload", "pk" ] pkDecoder
        |> requiredAt [ "payload", "action" ] string


{-| Deal with streams from a WebsocketDemultiplexer.
-}
streamDecoder : String -> (String -> model) -> model -> model
streamDecoder message okFunc fallback =
    case Json.Decode.decodeString (field "stream" string) message of
        Ok stream ->
            okFunc stream

        Err _ ->
            fallback
