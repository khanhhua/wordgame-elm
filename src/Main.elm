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
import Elements exposing (action, answerBar, navBar, px, stage, stats)


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
        , selectedWord = Nothing
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
                leftX = 30
                rightX = dumpster_inner_width - leftX * 4
                randomizer = Random.map2 ( \orderings xs ->
                        ( List.map3 (\a b word -> { ordering = a, position = ( b, 0 ), word = word })
                            orderings
                            xs
                            words )
                        |> List.sortBy (\item -> item.ordering)
                        |> List.map (\item ->
                            let
                                word = item.word
                            in
                                { word | position = item.position }
                            )
                    )
                    ( Random.list ( List.length words ) ( Random.float 0 1 ) )
                    ( Random.list ( List.length words ) ( Random.int leftX rightX ) )
            in
            ( { model
                | count = words |> List.length
                , words = words
                } , Random.generate UpdateWordPosition randomizer
            )
        UpdateWordPosition words ->
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
                    Just word -> [word] |> List.append model.stagedWords
                words = model.words |> List.tail |> Maybe.withDefault []
                status = if 0 == ( words |> List.length )
                    then ENDING
                    else model.status
            in
            ({ model
                | words = words
                , stagedWords = stagedWords
                , status = status
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
    { title = "WordGame - ELM 2021"
    , body =
        [ navBar
            [ action "Start" StartGame
            , action "Load" ( SelectFile "menschen/A2.txt" )
            ]
        , div [ class "container" ]
            [ div [ class "row" ]
                [ div
                    [ class "col-1 mx-auto"
                    , style "width" ( px dumpster_width )
                    ]
                    [ stats model.answers
                    , model |> stage
                        WordAnimationComplete
                        SelectAnswer
                        ( dumpster_width, (getHi model.screensize - 200) )
                    ]
                ]
            ]
        ]
    }


getY : ( Int, Int ) -> Int
getY position =
    let
        ( _, y ) = position
    in
    y
