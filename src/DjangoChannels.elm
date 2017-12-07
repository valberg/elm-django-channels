module DjangoChannels exposing (ModelBindingPayload, modelBindingDecoder, streamDecoder)

-- Core modules

import Json.Decode exposing (Decoder, string, field)


-- External modules

import Json.Decode.Pipeline exposing (decode, requiredAt)


type alias ModelBindingPayload dataType pkType =
    { model : String
    , data : dataType
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


streamDecoder : String -> (String -> model) -> model -> model
streamDecoder message okFunc fallback =
    case Json.Decode.decodeString (field "stream" string) message of
        Ok stream ->
            okFunc stream

        Err _ ->
            fallback
