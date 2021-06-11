module Common exposing (..)

import Browser.Dom as Browser
import Data exposing (Answer, GameStatus, Word)
import Time

release_frequency = 0.7
item_height = 232
overflow_limit = 5

type alias Model =
    { screensize: ( Float, Float )
    , status : GameStatus
    , count : Int
    , words : List Word
    , stagedWords : List Word
    , answers : List Answer
    }


type Msg
    = NoOp
    | SetScreenSize Browser.Viewport
    | Tick Time.Posix
    | SelectFile String
    | GotWordList ( List Word )
    | StartGame
    | ApplyRandomness ( List Word )
    | WordAnimationComplete Word
    | SelectAnswer Word String


dimension : Float -> Float -> ( Float, Float )
dimension w h = ( w, h )

getW : ( Float, Float ) -> Float
getW dim =
    let
        (w, _) = dim
    in
    w

getWi : ( Float, Float ) -> Int
getWi dim =
    let
        (w, _) = dim
    in
    floor w

getH : ( Float, Float ) -> Float
getH dim =
    let
        (_, h) = dim
    in
    h

getHi : ( Float, Float ) -> Int
getHi dim =
    let
        (_, h) = dim
    in
    floor h


updatePositions items =
    items |> List.foldl ( \item { words, index } ->
        if item.expired then { index = index, words = item :: words }
        else
            { index = index + 1
            , words = { item | position = ( 0, index * item_height ) } :: words
            }
    ) { words = [], index = 0 }
    |> (\{ words } -> words |> List.reverse )
