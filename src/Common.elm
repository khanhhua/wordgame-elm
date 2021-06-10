module Common exposing (..)

import Browser.Dom as Browser
import Data exposing (Answer, GameStatus, Word)
import Time

dumpster_width = 520
dumpster_inner_width = 500
release_frequency = 0.6


type alias Model =
    { screensize: ( Float, Float )
    , status : GameStatus
    , count : Int
    , words : List Word
    , stagedWords : List Word
    , selectedWord : Maybe Word
    , answers : List Answer
    }


type Msg
    = NoOp
    | SetScreenSize Browser.Viewport
    | Tick Time.Posix
    | SelectFile String
    | GotWordList ( List Word )
    | StartGame
    | UpdateWordPosition ( List ( Int, Int ) )
    | WordAnimationComplete Word
    | SelectWord Word
    | SelectAnswer String


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
