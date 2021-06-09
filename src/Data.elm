module Data exposing (..)

import Http exposing (Error(..))

deserializeGender : String -> Result Error Gender
deserializeGender gender =
    case gender of
        "MAS" -> Ok MAS
        "FEM" -> Ok FEM
        "NEU" -> Ok NEU
        _ -> Err ( BadBody gender )

toWordList : Result Error String -> List Word
toWordList content =
    content
    |> Result.withDefault ""
    |> String.split "\n"
    |> List.map ( \line ->
        case line |> String.split ";" of
            [ text, meaning, gender ] ->
                case deserializeGender gender of
                    Ok gender_ -> Just ( Word text meaning gender_ (0, 0) )
                    _ -> Nothing
            _ -> Nothing
        )
    |> List.filterMap identity

loadFileByName : ( List Word -> msg ) -> String -> Cmd msg
loadFileByName toMsg fileName =
    let
        fileUrl = "https://raw.githubusercontent.com/khanhhua/wordgame-data/master/" ++ fileName
    in
    Http.get
        { url = fileUrl
        , expect = Http.expectString ( toWordList >> toMsg)
        }


type Gender
    = MAS
    | FEM
    | NEU


type alias Word =
    { text : String
    , meaning : String
    , gender : Gender
    , position : ( Int, Int )
    }


type GameStatus
    = MENU
    | IN_GAME
    | ENDING
    | GAMEOVER


type alias Answer =
    { text : String
    , gender : Gender
    , correct : Bool
    }
