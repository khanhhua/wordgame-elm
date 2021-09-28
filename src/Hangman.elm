module Hangman exposing (..)

import Array
import Browser exposing (Document)
import Browser.Dom
import Browser.Navigation as Navigation
import Common exposing (GameStatus(..), HiddenWord, Model, Msg(..), Word, empty, initModel, release_frequency)
import Data exposing (loadCollectionsMetadata)
import Elements exposing (Action, action, gameoverElement, reportElement, stageSize)
import Html exposing (Html, div, input, p, text)
import Html.Attributes exposing (class, disabled, maxlength, value)
import Json.Decode as Json
import Random
import Task
import Time
import Url
import Html.Events exposing (keyCode, on, onInput)

type HMMsg
    = HMNoOp
    | ApplyRandomness ( List Word )
    | StartGame
    | PauseGame
    | ResumeGame
    | PickCharacter String
    | PickWrong
    | Tick Time.Posix

update : HMMsg -> Model -> ( Model, Cmd HMMsg )
update msg model =
    case msg of
        ApplyRandomness words ->
            ( { model
                | words = words
                , hiddenWord = List.head words
                    |> Maybe.map (\word ->
                        { text = word.text |> String.toLower
                        , displayedText = String.repeat ( String.length word.text ) "_"
                        , hint = word.meaning
                        })
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
        PickCharacter char ->
            let
                updateDisplayText word =
                    let
                        text_      = word.text |> String.toList |> Array.fromList
                        displayed_ = word.displayedText  |> String.toList |> Array.fromList
                    in
                    List.range 0 ((Array.length text_) - 1)
                        |> List.map (\index ->
                            ( Maybe.map2
                                (\t d ->
                                    if (String.fromChar t) == char
                                    then t
                                    else d
                                )
                                (Array.get index text_)
                                (Array.get index displayed_)
                            ) |> Maybe.withDefault '_'
                        )
                        |> String.fromList
                hiddenWord =  model.hiddenWord
                    |> Maybe.map (\word ->
                        let
                            displayedText_ = updateDisplayText word
                        in
                        { word | displayedText = displayedText_ })
            in
            ( { model | hiddenWord = hiddenWord }
            , Cmd.none )
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
    --if model.status == IN_GAME then
    --    Sub.batch [ Time.every (1/release_frequency * 1000) Tick ]
    --else
        Sub.none


appMenu : Model -> List (Action HMMsg)
appMenu model =
    if 0 == ( model.words |> List.length ) then []
    else
        ( case model.status of
            IN_GAME -> [ action "Pause" PauseGame ]
            PAUSED -> [ action "Resume" ResumeGame ]
            MENU -> [ action "Start" StartGame ]
            _ -> []
        )

gameStage : Model -> Html HMMsg
gameStage model =
    let
        currentWord = model.hiddenWord
    in
    div [ class "container" ]
        [ div [ class "row" ]
            ( if model.status == GAMEOVER
            then [ gameoverElement StartGame
                , reportElement model.answers
                ]
            else [
                currentWord
                |> Maybe.map (\word ->
                    div [ class "col-12" ]
                        [ hiddenBoxes PickWrong PickCharacter word
                        , div []
                            [ p [] [ text word.hint ]
                            ]
                        ]
                    )
                |> Maybe.withDefault empty
                ]
            )
        ]

hiddenBoxes : HMMsg -> (String -> HMMsg) -> HiddenWord -> Html HMMsg
hiddenBoxes incorrectChoice pickCharacter hiddenWord =
    let
        fnDisabled text i =
            text
            |> String.toList |> Array.fromList |> Array.get i
            |> Maybe.map ((/=) '_')
            |> Maybe.withDefault True
    in
    div [ class "hidden-boxes" ]
        [ div [ class "input-group hangman-hidden-word mx-auto" ]
            ( hiddenWord.displayedText
            |> String.toList
            |> List.indexedMap (\index char ->
                input
                    [ disabled (fnDisabled hiddenWord.displayedText index)
                    , maxlength 1
                    , class "form-control form-control-lg"
                    , value ( String.fromChar char )
                    , on "keydown" (Json.map (\enteredKey ->
                        let
                            charS : Maybe String
                            charS = hiddenWord.text
                                |> String.toList
                                |> Array.fromList
                                |> Array.get index
                                |> Maybe.map String.fromChar
                        in
                        charS
                        |> Maybe.map (\charS_ ->
                            if ((Debug.log "enteredKey" enteredKey) |> String.toLower) == charS_
                                then pickCharacter enteredKey
                                else incorrectChoice
                            )
                        |> Maybe.withDefault incorrectChoice
                    ) (Json.field "key" Json.string |> (Json.andThen (String.toLower >> Json.succeed) )))
                    ] []
                )
            )
        ]
