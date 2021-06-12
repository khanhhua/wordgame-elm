module Common exposing (..)

import Browser.Dom as Browser
import Time

release_frequency = 0.7
item_height = 232
overflow_limit = 5

type Msg
    = NoOp
    | SetScreenSize Browser.Viewport
    | Tick Time.Posix
    | SelectFile String
    | GotCollections ( List Collection )
    | GotWordList ( List Word )
    | StartGame
    | PauseGame
    | ResumeGame
    | ApplyRandomness ( List Word )
    | WordAnimationComplete Word
    | SelectAnswer Word String
    | ShowCollection Bool


initModel : Model
initModel =
    { screensize = dimension 0 0
    , status = MENU
    , count = 0
    , words = []
    , stagedWords = []
    , answers = []
    , showingCollections = False
    , collections = []
    }

resetGame : Model -> Model
resetGame model =
    { model
    | status = MENU
    , stagedWords = []
    , answers = []
    }

type alias Model =
    { screensize: ( Float, Float )
    , status : GameStatus
    , count : Int
    , words : List Word
    , stagedWords : List Word
    , answers : List Answer
    , showingCollections : Bool
    , collections : List Collection
    }

type Gender
    = MAS
    | FEM
    | NEU


type alias Package =
    { name : String
    , collections : List Collection
    }

type alias Collection =
    { packageId : String
    , name : String
    , file : String
    }

type alias Word =
    { text : String
    , meaning : String
    , gender : Gender
    , position : ( Int, Int )
    , expired : Bool
    }


type GameStatus
    = MENU
    | IN_GAME
    | PAUSED
    | ENDING
    | GAMEOVER


type alias Answer =
    { text : String
    , gender : Gender
    , correct : Bool
    }


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
