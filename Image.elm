module Image exposing
    ( Image
    , UploadStatus(..)
    , mImageDecoder
    , imageDecoder
    , uploadStatusDecoder
    )


import Json.Decode exposing (Decoder, int, oneOf, string, succeed, field)
import Json.Decode.Pipeline exposing (required)


type alias Image =
    { file : String
    , height : Int
    , width : Int
    }


type UploadStatus
    = Uploading Int
    | UploadFinished (Result String Image)


mImageDecoder : Decoder (Maybe Image)
mImageDecoder =
    Json.Decode.map3
        (\f h w -> Just (Image f h w))
        (field "file" string)
        (field "height" int)
        (field "width" int)


imageDecoder : Decoder Image
imageDecoder =
    succeed Image
        |> required "file" string
        |> required "height" int
        |> required "width" int


uploadStatusProgressDecoder : Decoder UploadStatus
uploadStatusProgressDecoder =
    Json.Decode.map
        (\progress -> Uploading progress)
        (field "progress" int)


uploadStatusSuccessDecoder : Decoder UploadStatus
uploadStatusSuccessDecoder =
    Json.Decode.map
        (\image -> UploadFinished (Ok image))
        imageDecoder


uploadStatusErrorDecoder : Decoder UploadStatus
uploadStatusErrorDecoder =
    Json.Decode.map
        (\err -> UploadFinished (Err err))
        (field "errorMsg" string)


uploadStatusDecoder : Decoder UploadStatus
uploadStatusDecoder =
    oneOf [ uploadStatusProgressDecoder, uploadStatusSuccessDecoder, uploadStatusErrorDecoder ]
