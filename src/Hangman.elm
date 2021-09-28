module Hangman exposing (..)

import Browser exposing (Document)
import Browser.Dom
import Browser.Navigation as Navigation
import Common exposing (GameStatus(..), Model, Msg(..), initModel, release_frequency)
import Data exposing (loadCollectionsMetadata)
import Elements exposing (stageSize)
import Html exposing (Html, div)
import Task
import Time
import Url

type HMMsg
    = HMNoOp
    | Tick Time.Posix

update : HMMsg -> Model -> ( Model, Cmd HMMsg )
update msg model =
    case msg of
        --Tick _ ->
        --    let
        --        next = model.words |> List.head
        --        stagedWords = case next of
        --            Nothing -> model.stagedWords
        --            Just word_ ->
        --                [word_]
        --                |> List.append model.stagedWords
        --                |> updatePositions
        --        overflow = overflow_limit < ( stagedWords |> List.filter ( .expired >> not ) |> List.length )
        --
        --        words = model.words |> List.tail |> Maybe.withDefault []
        --        status = if 0 == ( words |> List.length )
        --            then ENDING
        --            else model.status
        --    in
        --    ({ model
        --        | words = words
        --        , stagedWords = stagedWords
        --        , status = if overflow then GAMEOVER else status
        --        } , Cmd.none )
        _ -> ( model, Cmd.none )

subscriptions model =
    if model.status == IN_GAME then
        Sub.batch [ Time.every (1/release_frequency * 1000) Tick ]
    else
        Sub.none

gameStage : Model -> Html HMMsg
gameStage model =
    div [] []
