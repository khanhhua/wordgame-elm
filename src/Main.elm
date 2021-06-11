module Main exposing (..)

import Browser exposing (Document)
import Browser.Dom
import Browser.Navigation as Navigation

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Random exposing (initialSeed)
import Task
import Time
import Url

import Common exposing (..)
import Data exposing (..)
import Data exposing (GameStatus(..), Word)
import Elements exposing (action, gameoverElement, navBar, px, stage, stageSize, stats)


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , subscriptions = subscriptions
    , update = update
    , onUrlChange = (\_ -> NoOp)
    , onUrlRequest = (\_ -> NoOp)
    }


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    ( { screensize = dimension 0 0
        , count = 0
        , words = []
        , stagedWords = []
        , status = MENU
        , answers = []
        }, Task.perform SetScreenSize Browser.Dom.getViewport )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetScreenSize screen ->
            let
                size = ( screen.viewport.width, screen.viewport.height )
            in
            ( { model | screensize = size } , Cmd.none )
        SelectFile fileName ->
            ( model, loadFileByName GotWordList fileName )
        GotWordList words ->
            let
                count = List.length words
                ( stageW, stageH ) = stageSize model.screensize
                leftX = 10
                rightX = stageW - 180
                topY = 10
                bottomY = stageH - 190

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
            in
            ( { model
                | count = words |> List.length
                , words = words
                } , Random.generate ApplyRandomness randomizer
            )
        ApplyRandomness words ->
            ( { model
            | words = words
            } , Cmd.none
            )
        StartGame ->
            let
                next = model.words |> List.head
                stagedWords = case next of
                    Nothing -> model.stagedWords
                    Just word -> word |> List.singleton
                words = model.words |> List.tail |> Maybe.withDefault []
            in
            ( { model
                | words = words
                , stagedWords = stagedWords
                , status = IN_GAME
                }, Cmd.none )
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
            in
            ( { model
                | answers = answers
                , stagedWords = stagedWords
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


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.status == IN_GAME then
        Sub.batch [ Time.every (1/release_frequency * 1000) Tick ]
    else
        Sub.none


view : Model -> Document Msg
view model =
    let
        stageSize_ = stageSize model.screensize
        ( colW, _ ) = stageSize_
    in
    { title = "WordGame - ELM 2021"
    , body =
        [ model |> navBar
            [ action "Start" StartGame
            , action "Load" ( SelectFile "menschen/A2.txt" )
            ]
        , div [ class "container" ]
            [ div [ class "row" ]
                ( if model.status == GAMEOVER
                then [ gameoverElement ]
                else
                    [ model |> stage
                        WordAnimationComplete
                        SelectAnswer
                        stageSize_
                    ]
                )
            ]
        ]
    }

