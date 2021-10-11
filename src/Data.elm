module Data exposing (..)

import Common exposing (Collection, Tag(..), Msg(..), Package, Word)
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


loadCollectionsMetadata : ( List Collection -> Msg a) -> Cmd (Msg a)
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
                            Err _ -> NoOp
                    )
                    ( decodePackage packageId )
                }
        ) )

deserializeTags : String -> Result Error (List Tag)
deserializeTags tags =
    tags
    |> String.split ":"
    |> List.map tagFromString
    |> List.foldl (\item acc ->
        case acc of
            Ok list -> case item of
                Err innerErr -> Err ( BadBody innerErr )
                Ok tag -> Ok (tag :: list)
            err -> err
        ) (Ok [])

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
            [ text, meaning, tags ] ->
                case deserializeTags tags of
                    Ok tags_ -> Just ( Word text meaning tags_ (0, 0) False )
                    _ -> Nothing
            _ -> Nothing
        )
    |> List.filterMap identity

tagFromString : String -> Result String Tag
tagFromString s =
    case s of
        "MAS" -> Ok MAS
        "FEM" -> Ok FEM
        "NEU" -> Ok NEU
        "PLU" -> Ok PLU
        "NUM" -> Ok NUM
        "PRO" -> Ok PRO
        "ADJ"  -> Ok ADJ
        "ADV"  -> Ok ADV
        "VER"  -> Ok VER
        "PREP" -> Ok PREP
        "KON"  -> Ok KON
        "ART"  -> Ok ART
        other -> Err ("ValueError: " ++ other)
