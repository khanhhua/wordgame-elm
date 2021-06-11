module Data exposing (..)

import Common exposing (Collection, Gender(..), Msg(..), Package, Word)
import Http exposing (Error(..))
import Json.Decode as D exposing (Decoder)

collections =
    [ "internet"
    , "menschen"
    , "pons"
    ]


withBaseUrl : String -> String
withBaseUrl path =
    "https://raw.githubusercontent.com/khanhhua/wordgame-data/master/" ++ path


loadFileByName : ( List Word -> msg ) -> String -> Cmd msg
loadFileByName toMsg fileName =
    let
        fileUrl = fileName |> withBaseUrl
    in
    Http.get
        { url = fileUrl
        , expect = Http.expectString ( toWordList >> toMsg)
        }


loadCollectionsMetadata : ( List Collection -> Msg ) -> Cmd Msg
loadCollectionsMetadata toMsg =
    let
        fileUrls = List.map ( \name -> ( name, name ++ "/package.json" |> withBaseUrl )) collections
    in
    Cmd.batch
        ( fileUrls |> List.map ( \( packageId, fileUrl )->
            Http.get
                { url = fileUrl
                , expect = Http.expectJson
                    ( \result ->
                        case result of
                            Ok package -> toMsg package.collections
                            Err error ->
                                Debug.log ( "ERROR: " ++ ( Debug.toString error) )
                                NoOp
                    )
                    ( decodePackage packageId )
                }
        ) )

deserializeGender : String -> Result Error Gender
deserializeGender gender =
    case gender of
        "MAS" -> Ok MAS
        "FEM" -> Ok FEM
        "NEU" -> Ok NEU
        _ -> Err ( BadBody gender )

decodePackage : String -> Decoder Package
decodePackage packageId =
    D.map2 Package
        ( D.field "name" D.string )
        ( D.field "collections" (
            D.list ( decodeCollection packageId ) )
        )

decodeCollection : String -> Decoder Collection
decodeCollection packageId =
    D.map3 Collection
        ( D.succeed packageId )
        ( D.field "name" D.string )
        ( D.field "file" D.string )

toWordList : Result Error String -> List Word
toWordList content =
    content
    |> Result.withDefault ""
    |> String.split "\n"
    |> List.map ( \line ->
        case line |> String.split ";" of
            [ text, meaning, gender ] ->
                case deserializeGender gender of
                    Ok gender_ -> Just ( Word text meaning gender_ (0, 0) False )
                    _ -> Nothing
            _ -> Nothing
        )
    |> List.filterMap identity
