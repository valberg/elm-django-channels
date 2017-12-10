module DjangoChannels.Binding
    exposing
        ( BindingStreamHandler
        , handleBindingStream
        , defaultCreate
        , defaultUpdate
        , defaultDelete
        , createInstance
        , updateInstance
        , deleteInstance
        )

{-|

# Definition
@docs BindingStreamHandler

# Utilities
@docs handleBindingStream

# Default operations
@docs defaultCreate, defaultUpdate, defaultDelete

# Functions for sending data back to server
@docs createInstance, updateInstance, deleteInstance

-}

-- Core modules

import Json.Decode exposing (Decoder, string, field, list)
import Json.Encode


-- External modules

import Json.Decode.Pipeline exposing (decode, requiredAt)
import WebSocket


{-| BindingStreamHandler defines how we should deal with a specific WebsocketBinding stream.
-}
type alias BindingStreamHandler pkType instanceType =
    { websocketServer : String
    , streamName : String
    , instanceDecoder : Decoder instanceType
    , instanceEncoder : instanceType -> Json.Encode.Value
    , pkDecoder : Decoder pkType
    , pkEncoder : pkType -> Json.Encode.Value
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


type ServerAction
    = Create
    | Update
    | Delete


actionToString : ServerAction -> String
actionToString action =
    case action of
        Create ->
            "create"

        Update ->
            "update"

        Delete ->
            "delete"


{-| -}
createInstance : BindingStreamHandler pkType instanceType -> instanceType -> Cmd msg
createInstance streamHandler instance =
    sendToServer
        streamHandler
        Create
        Nothing
        (Just instance)


{-| -}
updateInstance : BindingStreamHandler pkType instanceType -> pkType -> instanceType -> Cmd msg
updateInstance streamHandler pk instance =
    sendToServer
        streamHandler
        Update
        (Just pk)
        (Just instance)


{-| -}
deleteInstance : BindingStreamHandler pkType instanceType -> pkType -> Cmd msg
deleteInstance streamHandler pk =
    sendToServer
        streamHandler
        Delete
        (Just pk)
        Nothing


sendToServer : BindingStreamHandler pkType instanceType -> ServerAction -> Maybe pkType -> Maybe instanceType -> Cmd msgType
sendToServer streamHandler action maybePk maybeInstance =
    let
        dataField =
            case maybeInstance of
                Just instance ->
                    [ ( "data", streamHandler.instanceEncoder instance ) ]

                Nothing ->
                    []

        actionField =
            [ ( "action", Json.Encode.string <| actionToString action ) ]

        pkField =
            case maybePk of
                Just pk ->
                    [ ( "pk", streamHandler.pkEncoder pk ) ]

                Nothing ->
                    []

        payload =
            case action of
                Create ->
                    actionField ++ dataField

                Update ->
                    pkField ++ actionField ++ dataField

                Delete ->
                    pkField ++ actionField
    in
        WebSocket.send streamHandler.websocketServer
            (Json.Encode.encode 0
                (Json.Encode.object
                    [ ( "stream", Json.Encode.string streamHandler.streamName )
                    , ( "payload", Json.Encode.object payload )
                    ]
                )
            )
