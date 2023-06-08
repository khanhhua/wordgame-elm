module Data exposing (..)

import Array
import Common exposing (Collection, Tag(..), Msg(..), Package, Word)
import Http exposing (Error(..))
import Json.Decode as D exposing (Decoder)

collections =
    [ "internet"
    , "menschen"
    , "pons"
    , "sicher"
    , "focus"
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
            [ stemForm, meaning, tags ] ->
                case deserializeTags tags of
                    Ok tags_ ->
                        let
                            words = if List.member PLU tags_ || List.member MAS tags_ || List.member FEM tags_ || List.member NEU tags_
                                then extractNouns stemForm meaning tags_
                                else if List.member VER tags_
                                then extractVerbs stemForm meaning tags_
                                else extractOthers stemForm meaning tags_
                        in
                        Just words
                    _ -> Nothing
            _ -> Nothing
        )
    |> List.filterMap identity
    |> List.concat

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

extractNouns : String -> String -> List Tag -> List Word
extractNouns stemForm meaning tags =
    let
        createPlural : String -> String -> String
        createPlural stem_ endingPattern =
            let
                prefix = String.slice 0 1 endingPattern
                suffix = String.dropLeft 1 endingPattern
            in
            case prefix of
                "-" -> stem_ ++ suffix
                ":" ->
                    let
                        indexToUpdate =
                            [ "au"
                             , "a"
                             , "A"
                             , "o"
                             , "O"
                             , "u"
                             , "U"
                             ]
                             |> List.map (\item -> lastIndexOf item stem_)
                             |> List.filter ((/=)(-1))
                             |> List.minimum
                             |> Maybe.withDefault -1
                    in
                    ( stem_
                        |> String.toList
                        |> List.indexedMap (\index char ->
                            if index == indexToUpdate
                            then case char of
                                'a' -> 'ä'
                                'A' -> 'Ä'
                                'o' -> 'ö'
                                'O' -> 'Ö'
                                'u' -> 'ü'
                                'U' -> 'Ü'
                                other -> other
                            else char
                            )
                        |> String.fromList
                    ) ++ suffix
                _ -> ""

        (stem, ending) = case String.split "," stemForm of
            [a, b] -> (Just a, Just b)
            [a] -> (Just a, Nothing)
            _ -> (Nothing, Nothing)
    in
    if ending /= Nothing && (List.length tags == 2 || List.length tags == 1)
    then (
        Maybe.map3 (\stem_ ending_ otherTag_ ->
            [ Word stem_ meaning [otherTag_] (0, 0) False
            , Word (createPlural stem_ ending_) meaning [PLU] (0, 0) False
            ] )
            stem
            ending
            (List.filter ((/=)PLU) tags |> List.head)
        |> Maybe.withDefault []
    )
    else stem
        |> Maybe.map (\stem_ -> [ Word stem_ meaning tags (0, 0) False ])
        |> Maybe.withDefault []


extractVerbs : String -> String -> List Tag -> List Word
extractVerbs stemForm meaning tags =
    let
        (mbV0, mbV2, mbV3) = case String.split "," stemForm of
            [a, b, c] -> (Just a, Just b, Just c)
            [a] -> (Just a, Nothing, Nothing)
            _ -> (Nothing, Nothing, Nothing)
    in
    if mbV0 == Nothing then []
    else if (mbV0 /= Nothing) && (mbV2 /= Nothing) && (mbV3 /= Nothing) then
        Maybe.map3 (\v0 v2 v3 ->
            [ Word v0 meaning tags (0, 0) False
            , Word v2 v0 tags (0, 0) False
            , Word v3 v0 tags (0, 0) False
            ])
            mbV0 mbV2 mbV3
        |> Maybe.withDefault []
    else (
        mbV0
        |> Maybe.map (\v0 -> [Word v0 meaning tags (0, 0) False])
        |> Maybe.withDefault []
    )

extractOthers : String -> String -> List Tag -> List Word
extractOthers stemForm meaning tags = [Word stemForm meaning tags (0, 0) False]


lastIndexOf : String -> String -> Int
lastIndexOf needle haystack =
    ( String.indices (String.slice 0 1 needle) haystack
        |> List.foldl (\index acc ->
            if ( needle
                |> String.toList
                |> List.indexedMap (\i c -> (i, c))
                |> List.foldl (\(i,c) acc2 ->
                    if not acc2 then acc2
                    else (acc2 && (String.slice (i + index) (i + index + 1) haystack) == String.fromList [c])
                    ) True
            ) then index
            else acc
            ) -1
    )
