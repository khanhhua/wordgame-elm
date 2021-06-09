module Common exposing (..)

import Data exposing (Answer, GameStatus, Word)
import Time

dumpster_width = "520"
dumpster_inner_width = "500"
release_frequency = 0.6


type alias Model =
    { status : GameStatus
    , count : Int
    , words : List Word
    , stagedWords : List Word
    , selectedWord : Maybe Word
    , answers : List Answer
    }


type Msg
    = NoOp
    | Tick Time.Posix
    | SelectFile String
    | GotWordList ( List Word )
    | StartGame
    | UpdateWordPosition ( List ( Int, Int ) )
    | WordAnimationComplete Word
    | SelectWord Word
    | SelectAnswer String
