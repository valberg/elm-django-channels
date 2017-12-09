module DjangoChannels
    exposing
        ( streamDemultiplexer
        , BindingStreamHandler
        , handleBindingStream
        , defaultCreate
        , defaultUpdate
        , defaultDelete
        , InitialStreamHandler
        , handleInitialStream
        )

{-|

Decoders and such to ease communication with django channels.

elm-django-channels is an oppinionated way to use communicate with Django Channels from Elm.

# Definition
@docs BindingStreamHandler

# Utilities
@docs streamDemultiplexer, handleBindingStream

# Default operations
@docs defaultCreate, defaultUpdate, defaultDelete

-}

-- Core modules

import Json.Decode exposing (Decoder, string, field, list)


-- External modules

import Json.Decode.Pipeline exposing (decode, requiredAt)


type alias ModelBindingPayload instanceType pkType =
    { model : String
    , data : instanceType
    , pk : pkType
    , action : String
    }


modelBindingDecoder : Decoder a -> Decoder b -> Decoder (ModelBindingPayload a b)
modelBindingDecoder dataDecoder pkDecoder =
    decode ModelBindingPayload
        |> requiredAt [ "payload", "model" ] string
        |> requiredAt [ "payload", "data" ] dataDecoder
        |> requiredAt [ "payload", "pk" ] pkDecoder
        |> requiredAt [ "payload", "action" ] string


{-| Deal with streams from a WebsocketDemultiplexer.
-}
streamDemultiplexer : String -> (String -> streamType) -> streamType -> streamType
streamDemultiplexer data stringToStream notFoundStream =
    case Json.Decode.decodeString (field "stream" string) data of
        Ok stream ->
            stringToStream stream

        Err _ ->
            notFoundStream


type alias InitialStreamHandler pkType instanceType =
    { instanceDecoder : Decoder instanceType
    , pkDecoder : Decoder pkType
    }


{-| BindingStreamHandler defines how we should deal with a specific WebsocketBinding stream.
-}
type alias BindingStreamHandler pkType instanceType =
    { instanceDecoder : Decoder instanceType
    , pkDecoder : Decoder pkType
    , createFunc : instanceType -> pkType -> List ( pkType, instanceType ) -> List ( pkType, instanceType )
    , updateFunc : instanceType -> pkType -> List ( pkType, instanceType ) -> List ( pkType, instanceType )
    , deleteFunc : pkType -> List ( pkType, instanceType ) -> List ( pkType, instanceType )
    }


{-| Handle data on a given BindingStreamHandler.
-}
handleBindingStream : BindingStreamHandler pkType instanceType -> String -> List ( pkType, instanceType ) -> List ( pkType, instanceType )
handleBindingStream streamHandler data instances =
    let
        decoder =
            modelBindingDecoder streamHandler.instanceDecoder streamHandler.pkDecoder
    in
        case Json.Decode.decodeString decoder data of
            Ok decodedData ->
                case decodedData.action of
                    "create" ->
                        streamHandler.createFunc decodedData.data decodedData.pk instances

                    "update" ->
                        streamHandler.updateFunc decodedData.data decodedData.pk instances

                    "delete" ->
                        streamHandler.deleteFunc decodedData.pk instances

                    _ ->
                        instances

            Err _ ->
                instances


handleInitialStream : InitialStreamHandler pkType instanceType -> String -> List ( pkType, instanceType )
handleInitialStream streamHandler data =
    let
        decoder =
            modelPayloadDecoder streamHandler.instanceDecoder streamHandler.pkDecoder
    in
        case Json.Decode.decodeString (field "payload" (list decoder)) data of
            Ok instances ->
                List.map (\x -> ( x.pk, x.data )) instances

            Err error ->
                let
                    _ =
                        Debug.log "error" error
                in
                    []


type alias ModelPayload instanceType pkType =
    { model : String
    , data : instanceType
    , pk : pkType
    }


modelPayloadDecoder : Decoder a -> Decoder b -> Decoder (ModelPayload a b)
modelPayloadDecoder dataDecoder pkDecoder =
    (decode ModelPayload
        |> requiredAt [ "model" ] string
        |> requiredAt [ "data" ] dataDecoder
        |> requiredAt [ "pk" ] pkDecoder
    )



-- Default create, update, delete functions


{-| Default function for creating an instance.
-}
defaultCreate : instanceType -> pkType -> List ( pkType, instanceType ) -> List ( pkType, instanceType )
defaultCreate instance pk instances =
    ( pk, instance ) :: instances


{-| Default function for updating an instance.
-}
defaultUpdate : instanceType -> pkType -> List ( pkType, instanceType ) -> List ( pkType, instanceType )
defaultUpdate instance pk instances =
    List.map
        (\( curPk, x ) ->
            if pk == curPk then
                ( curPk, instance )
            else
                ( curPk, x )
        )
        instances


{-| Default function for deleting an instance given a pk.
-}
defaultDelete : pkType -> List ( pkType, instanceType ) -> List ( pkType, instanceType )
defaultDelete pk instances =
    List.filter (\( curPk, _ ) -> pk /= curPk) instances
