module Common exposing (..)

import Browser.Dom as Browser
import Html exposing (Html, text)

release_frequency = 0.7
item_height = 232
overflow_limit = 5

type Msg a
    = NoOp
    | SetScreenSize Browser.Viewport
    | SelectFile String
    | GotCollections ( List Collection )
    | GotWordList ( List Word )
    | ShowCollection Bool
    | SelectGame Int
    | StartGame
    | PauseGame
    | ResumeGame
    | GameMsg a


resetGame : Model g -> Model g
resetGame model = model

--
--type Game
--    = GameGenderRace
--    | GameHangMan


type alias Model g =
    { screensize: ( Float, Float )
    , status : GameStatus
    , gameModel : g
    , count : Int
    , words : List Word
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


initModel : g -> Model g
initModel gameModel =
    { screensize = dimension 0 0
    , status = MENU
    , gameModel = gameModel
    , count = 0
    , showingCollections = False
    , collections = []
    , words = []
    }

modelMap : (g -> Maybe a) -> Model g -> Model (Maybe a)
modelMap f m =
    let
        default = { screensize = m.screensize
            , status = m.status
            , gameModel = Nothing
            , count = m.count
            , showingCollections = m.showingCollections
            , collections = m.collections
            , words = m.words
            }
    in
    case m.status of
        MENU -> default
        _ -> { default
             | gameModel = (f m.gameModel)
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


genderToString : Gender -> String
genderToString gender =
    case gender of
        MAS -> "MAS"
        FEM -> "FEM"
        NEU -> "NEU"

empty : Html msg
empty = text ""
