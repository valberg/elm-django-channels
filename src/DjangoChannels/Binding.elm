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

    import DjangoChannels.Binding as DCB

    websocketServer = "ws://localhost:8000/"

    type alias Todo =
        { description : String
        , isDone : Bool
        }

    todoDecoder : Decoder Todo
    todoDecoder =
        decode Todo
            |> required "description" string
            |> required "is_done" bool

    todoEncoder : Todo -> Json.Encode.Value
    todoEncoder instance =
        Json.Encode.object
            [ ( "description", Json.Encode.string instance.description )
            , ( "is_done", Json.Encode.bool instance.isDone )
            ]

    todoStreamHandler : DCB.BindingStreamHandler String Todo
    todoStreamHandler =
        DCB.BindingStreamHandler
            websocketServer
            "todo"
            todoDecoder
            todoEncoder
            string
            Json.Encode.string
            DCB.defaultCreate
            DCB.defaultUpdate
            DCB.defaultDelete

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


{-| Handle data using a BindingStreamHandler.

    import DjangoChannels.Binding as DCB

    --

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            ...

            HandleWebSocket data ->
                case streamDemultiplexer data stringToStream NotFoundStream of
                    TodoStream ->
                        let
                            todos =
                                DCB.handleBindingStream todoStreamHandler data model.todos
                        in
                            ( { model | todos = todos }
                            , Cmd.none
                            )

            ...


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


{-| Default function for creating an instance. Used as "createFunc" in a BindingStreamHandler to simply cons the pk-instance-pair onto the list of instances.
-}
defaultCreate : instanceType -> pkType -> List ( pkType, instanceType ) -> List ( pkType, instanceType )
defaultCreate instance pk instances =
    ( pk, instance ) :: instances


{-| Default function for updating an instance. Used as "updateFunc" in a BindingStreamHandler to map over the list of instances and update the one matching the given pk.
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


{-| Default function for deleting an instance given a pk. Used as "deleteFunc in a BindingStreamHandler to filter out the instance with the given pk.
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


{-| Sends the instance with the "create" action to the websocket server.
-}
createInstance : BindingStreamHandler pkType instanceType -> instanceType -> Cmd msg
createInstance streamHandler instance =
    sendToServer
        streamHandler
        Create
        Nothing
        (Just instance)


{-| Sends the pk and instance with the "update" action to the websocket server.
-}
updateInstance : BindingStreamHandler pkType instanceType -> pkType -> instanceType -> Cmd msg
updateInstance streamHandler pk instance =
    sendToServer
        streamHandler
        Update
        (Just pk)
        (Just instance)


{-| Sends the pk with the "delete" action to the websocket server.
-}
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
