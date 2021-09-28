module GenderRace exposing (..)

import Browser exposing (Document)
import Browser.Dom
import Browser.Navigation as Navigation

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Random
import Task
import Time
import Url

import Common exposing (..)
import Data exposing (..)
import Elements exposing (Action, action, collectionListElement, gameoverElement, navBar, reportElement, stage, stageSize)

type GRMsg
    = ApplyRandomness (List Word)
    | StartGame
    | PauseGame
    | ResumeGame
    | WordAnimationComplete Word
    | SelectAnswer Word String
    | Tick Time.Posix
    | GrNoOp


update : GRMsg -> Model -> ( Model, Cmd GRMsg)
update msg model =
    case msg of
        ApplyRandomness words ->
            ( { model
                | words = words
                }
            , Cmd.none
            )
        StartGame ->
            let
                count = List.length model.words

                randomizer = Random.map ( \orderings ->
                    ( List.map2 (\ordering word  -> { ordering = ordering, position = ( 0, 0 ), word = word })
                        orderings
                        words
                    )
                    |> List.sortBy (\item -> item.ordering)
                    |> List.map (\item ->
                        let
                            word = item.word
                        in
                            { word | position = item.position }
                        )
                    )
                    ( Random.list count ( Random.float 0 1 ) )

                stagedWords = []
                words = if 0 == ( model.words |> List.length )
                    then model.stagedWords |> List.map ( \item -> { item | expired = False })
                    else model.words
            in
            ( { model
                | words = words
                , stagedWords = stagedWords
                , status = IN_GAME
                , answers = []
                }, Random.generate ApplyRandomness randomizer )
        PauseGame ->
            ( { model | status = PAUSED }, Cmd.none )
        ResumeGame ->
            ( { model | status = IN_GAME }, Cmd.none )
        WordAnimationComplete key ->
            let
                count = model.count - 1
                status = if 0 == count then GAMEOVER else model.status
                stagedWords = model.stagedWords
                    |> List.map (\word ->
                        if word == key then
                            { word | expired = True }
                        else
                            word
                    )
                    |> updatePositions
            in
            ( { model
                | count = count
                , stagedWords = stagedWords
                , status = status
                } , Cmd.none)
        SelectAnswer word answerText ->
            let
                answers =
                    if ( word.gender == MAS && answerText == "DER" )
                        || ( word.gender == FEM && answerText == "DIE" )
                        || ( word.gender == NEU && answerText == "DAS" )
                    then
                         ( Answer word.text word.gender True ) :: model.answers
                    else
                         ( Answer word.text word.gender False ) :: model.answers
                stagedWords = model.stagedWords
                    |> List.map (\word_ ->
                        if word_ == word then
                            { word_ | expired = True }
                        else
                            word_
                    )
                    |> updatePositions
                status = if
                        0 == ( stagedWords |> List.filter ( .expired >> not ) |> List.length )
                        && 0 == ( model.words |> List.length )
                    then GAMEOVER
                    else model.status
            in
            ( { model
                | answers = answers
                , stagedWords = stagedWords
                , status = status
                }, Cmd.none )
        Tick _ ->
            let
                next = model.words |> List.head
                stagedWords = case next of
                    Nothing -> model.stagedWords
                    Just word_ ->
                        [word_]
                        |> List.append model.stagedWords
                        |> updatePositions
                overflow = overflow_limit < ( stagedWords |> List.filter ( .expired >> not ) |> List.length )

                words = model.words |> List.tail |> Maybe.withDefault []
                status = if 0 == ( words |> List.length )
                    then ENDING
                    else model.status
            in
            ({ model
                | words = words
                , stagedWords = stagedWords
                , status = if overflow then GAMEOVER else status
                } , Cmd.none )
        _ -> ( model, Cmd.none )


subscriptions : Model -> Sub GRMsg
subscriptions model =
    if model.status == IN_GAME then
        Sub.batch [ Time.every (1/release_frequency * 1000) Tick ]
    else
        Sub.none


appMenu : Model -> List (Action GRMsg)
appMenu model =
    if 0 == ( model.words |> List.length ) then []
    else
        ( case model.status of
            IN_GAME -> [ action "Pause" PauseGame ]
            PAUSED -> [ action "Resume" ResumeGame ]
            MENU -> [ action "Start" StartGame ]
            _ -> []
        )


gameStage : Model -> Html GRMsg
gameStage model =
    let
        stageSize_ = stageSize model.screensize
        ( colW, _ ) = stageSize_
    in
    div [ class "container" ]
        [ div [ class "row" ]
            ( if model.status == GAMEOVER
            then [ gameoverElement StartGame
                , reportElement model.answers
                ]
            else
                [ model |> stage
                    WordAnimationComplete
                    SelectAnswer
                    stageSize_
                ]
            )
        ]
