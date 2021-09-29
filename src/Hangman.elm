module Hangman exposing (..)

import Array
import Common exposing (GameStatus(..), Model, Msg(..), Word, empty)
import Elements exposing (Action, action, gameoverElement, reportElement, stageSize)
import Html exposing (Html, div, input, p, text)
import Html.Attributes exposing (class, disabled, maxlength, value)
import Json.Decode as Json
import Random
import Set exposing (Set)
import Time
import Html.Events exposing (on)

type HMMsg
    = HMNoOp
    | ApplyRandomness ( List Word )
    | LoadWords ( List Word )
    | StartGame
    | PauseGame
    | ResumeGame
    | PickCharacter String
    | PickWrong
    | Tick Time.Posix

type alias HiddenWord =
    { text : String
    , displayedIndices: List Bool
    , hint : String
    }

type alias HMModel =
    { hiddenWord : Maybe HiddenWord
    , words : List Word
    , stagedWords : List Word -- Used words
    , status : GameStatus
    , answers : List Word
    , gallowStatus : List GallowStatus
    }


type GallowStatus
    = Noose
    | Head
    | Body
    | LeftHand
    | RightHand
    | LeftLeg
    | RightLeg
    | Dead


executionSequence = [ Noose, Head, Body, LeftHand, RightHand, LeftLeg, RightLeg, Dead ] |> Array.fromList


initModel : HMModel
initModel =
    { hiddenWord = Nothing
    , words = []
    , stagedWords = []
    , status = MENU
    , answers = []
    , gallowStatus = []
    }


update : HMMsg -> HMModel -> ( HMModel, Cmd HMMsg )
update msg model =
    case msg of
        LoadWords words ->
            ( { model | words = words }, Cmd.none )
        ApplyRandomness words ->
            ( { model
                | words = words
                , hiddenWord = List.head words
                    |> Maybe.map (\word ->
                        { text = word.text |> String.toLower
                        , displayedIndices = List.repeat ( String.length word.text ) False
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
                updateDisplayIndices : HiddenWord -> List Bool
                updateDisplayIndices word =
                    let
                        text_      = word.text |> String.toList |> Array.fromList
                        displayed_ = word.displayedIndices |> Array.fromList
                    in
                    List.range 0 ((Array.length text_) - 1)
                        |> List.map (\index ->
                            ( Maybe.map2
                                (\t currentlyDisplayed ->
                                    if (String.fromChar t) == char
                                    then True
                                    else currentlyDisplayed
                                )
                                (Array.get index text_)
                                (Array.get index displayed_)
                            ) |> Maybe.withDefault False
                        )
                hiddenWord = model.hiddenWord
                    |> Maybe.map (\word ->
                        { word | displayedIndices = updateDisplayIndices word }
                    )
            in
            ( { model | hiddenWord = hiddenWord }
            , Cmd.none )
        PickWrong ->
            let
                nextStatus = executionSequence
                    |> Array.get (List.length model.gallowStatus)
                gallowStatus = case nextStatus of
                    Just status -> model.gallowStatus |> (::) status
                    Nothing -> model.gallowStatus
            in
            ( { model | gallowStatus = gallowStatus }
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


appMenu : HMModel -> List (Action HMMsg)
appMenu model =
    if 0 == ( model.words |> List.length ) then []
    else
        ( case model.status of
            IN_GAME -> [ action "Pause" PauseGame ]
            PAUSED -> [ action "Resume" ResumeGame ]
            MENU -> [ action "Start" StartGame ]
            _ -> []
        )

gameStage : HMModel -> Html HMMsg
gameStage model =
    let
        currentWord = model.hiddenWord
    in
    div [ class "container" ]
        [ div [ class "row" ]
            ( if model.status == GAMEOVER
            then [ gameoverElement StartGame
                --, reportElement model.answers
                ]
            else [
                currentWord
                |> Maybe.map (\word ->
                    div [ class "col-12" ]
                        [ hiddenBoxes PickWrong PickCharacter word
                        , div []
                            [ p [ class "text-center display-2" ] [ text word.hint ]
                            ]
                        ]
                    )
                |> Maybe.withDefault empty
                , gallowElement model.gallowStatus
                ]
            )
        ]

hiddenBoxes : HMMsg -> (String -> HMMsg) -> HiddenWord -> Html HMMsg
hiddenBoxes incorrectChoice pickCharacter hiddenWord =
    let
        fnDisabled i =
            hiddenWord.displayedIndices
            |> Array.fromList
            |> Array.get i
        fnVisibleChatAt i =
            hiddenWord.displayedIndices
            |> Array.fromList
            |> Array.get i
            |> Maybe.andThen (\visible ->
                if not visible then Just "_"
                else hiddenWord.text
                    |> String.toList
                    |> Array.fromList
                    |> Array.get i
                    |> Maybe.map String.fromChar
            )
        keydownDecoder = (Json.string |> Json.andThen (String.toLower >> Json.succeed))
    in
    div [ class "hidden-boxes" ]
        [ div [ class "input-group hangman-hidden-word mx-auto" ]
            ( hiddenWord.displayedIndices
            |> List.indexedMap (\index displayed ->
                input
                    [ disabled (fnDisabled index |> Maybe.withDefault True)
                    , maxlength 1
                    , class "form-control form-control-lg"
                    , value ( fnVisibleChatAt index |> Maybe.withDefault "_" )
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
                    ) (Json.field "key" keydownDecoder))
                    ] []
                )
            )
        ]


gallowElement : List GallowStatus -> Html msg
gallowElement stati =
    div [ class "gallow" ]
        ( stati |> List.foldl (\item acc ->
            let
                d = case item of
                    Noose -> div [ class "gallow-noose" ] []
                    Head -> div [ class "gallow-head" ] []
                    Body -> div [ class "gallow-body" ] []
                    LeftHand -> div [ class "gallow-left-hand" ] []
                    RightHand -> div [ class "gallow-right-hand" ] []
                    LeftLeg -> div [ class "gallow-left-leg" ] []
                    RightLeg -> div [ class "gallow-right-leg" ] []
                    Dead -> div [ class "gallow-dead" ] []
            in (d :: acc)
            ) []
        )
