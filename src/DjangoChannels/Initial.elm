module DjangoChannels.Initial
    exposing
        ( InitialStreamHandler
        , handleInitialStream
        )

{-|

# Definition
@docs InitialStreamHandler

# Utilities
@docs handleInitialStream

-}

-- Core modules

import Json.Decode exposing (Decoder, string, field, list)


-- External modules

import Json.Decode.Pipeline exposing (decode, requiredAt)


{-| -}
type alias InitialStreamHandler pkType instanceType =
    { instanceDecoder : Decoder instanceType
    , pkDecoder : Decoder pkType
    }


{-| -}
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
