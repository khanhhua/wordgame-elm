module Hangman exposing (..)

import Array
import Browser.Dom
import Common exposing (Answer, GameStatus(..), Model, Msg(..), Tag(..), Word, empty, filterWords)
import Elements exposing (Action, action, gameoverElement, navBar, reportElement, stageSize)
import Html exposing (Html, button, div, input, p, span, text)
import Html.Attributes exposing (class, disabled, id, maxlength, tabindex, value)
import Json.Decode as Json
import Random
import Task
import Time
import Html.Events exposing (on, onClick)

type HMMsg
    = ApplyRandomness ( List Word )
    | LoadWords ( List Word )
    | PickCharacter String
    | PickWrong
    | NextWord
    | Tick Time.Posix

type alias HiddenWord =
    { text : String
    , displayedIndices: List Bool
    , hint : String
    }

type alias HMModel =
    { showingResult : Bool
    , hiddenWord : Maybe HiddenWord
    , words : List Word
    , stagedWords : List Word -- Used words
    , answers : List Answer
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


executionSequence = [ Head, Body, LeftHand, RightHand, LeftLeg, RightLeg, Noose, Dead ] |> Array.fromList


initModel : HMModel
initModel =
    { showingResult = False
    , hiddenWord = Nothing
    , words = []
    , stagedWords = []
    , answers = []
    , gallowStatus = []
    }


update : Msg HMMsg -> HMModel -> ( HMModel, Cmd (Msg HMMsg) )
update msg model =
    case msg of
        GameMsg (LoadWords words) ->
            ( { initModel | words = words |> filterWords [MAS, FEM, NEU, VER, ADJ] }, Cmd.none )
        GameMsg (ApplyRandomness words) ->
            let
                  head = words |> List.head
                  tail = words |> List.tail
                  stagedWords = head |> Maybe.map (\w -> w :: model.stagedWords ) |> Maybe.withDefault []
            in
            ( { model
                | stagedWords = stagedWords
                , words = tail |> Maybe.withDefault []
                , hiddenWord = head
                    |> Maybe.map (\word ->
                        { text = word.text |> String.toLower
                        , displayedIndices = List.repeat ( String.length word.text ) False
                        , hint = word.meaning
                        })
                }
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "stage")
            )
        StartGame ->
            let
                count = List.length model.words + List.length model.stagedWords
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
                    then model.stagedWords
                    else model.words
            in
            ( { initModel
                | words = words
                , stagedWords = stagedWords
                , answers = []
                }, Random.generate (GameMsg << ApplyRandomness) randomizer )
        GameMsg (PickCharacter char) ->
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
                isCorrect = hiddenWord
                    |> Maybe.map (\word ->
                        0 == ( word.displayedIndices
                                |> List.filter ((==) False)
                                |> List.length
                            )
                    )
                    |> Maybe.withDefault False
                answers = if isCorrect
                    then model.stagedWords
                        |> List.head
                        |> Maybe.map (\word -> { text = word.text
                                               , tags = word.tags
                                               , correct = True
                                               } :: model.answers
                           )
                        |> Maybe.withDefault model.answers
                    else model.answers

                --status = if isCorrect && (List.length model.words == 0)
                --    then GAMEOVER
                --    else model.status
            in
            ( { model
                | hiddenWord = hiddenWord
                , answers = answers
            }
            , Cmd.none )
        GameMsg PickWrong ->
            let
                nextStatus = executionSequence
                    |> Array.get (List.length model.gallowStatus)
                gallowStatus = case nextStatus of
                    Just status -> model.gallowStatus |> (::) status
                    Nothing -> model.gallowStatus
                isLost = ( List.length gallowStatus ) == ( Array.length executionSequence )
                answers = if isLost
                    then model.stagedWords
                        |> List.head
                        |> Maybe.map (\word -> { text = word.text
                                               , tags = word.tags
                                               , correct = False
                                               } :: model.answers
                           )
                       |> Maybe.withDefault model.answers
                   else model.answers
                --status_ = if isLost && (List.length model.words == 0)
                --    then GAMEOVER
                --    else model.status
            in
            ( { model
                | gallowStatus = gallowStatus
                , answers = answers
            }
            , Cmd.none )
        GameMsg NextWord ->
            let
                head = model.words |> List.head
                tail = model.words |> List.tail
                stagedWords = head |> Maybe.map (\w -> w :: model.stagedWords ) |> Maybe.withDefault []
                showingResult = head == Nothing
                gameCmd = if showingResult
                    then Task.perform identity (Task.succeed CompleteGame)
                    else (Task.attempt (\_ -> NoOp) (Browser.Dom.focus "stage"))
            in
            ( { model
                | stagedWords = stagedWords
                , words = tail |> Maybe.withDefault []
                , showingResult = showingResult
                , hiddenWord = head
                    |> Maybe.map (\word ->
                        { text = word.text |> String.toLower
                        , displayedIndices = List.repeat ( String.length word.text ) False
                        , hint = word.meaning
                        })
                , gallowStatus = []
                }
            , gameCmd
            )
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


view : Model (Maybe HMModel) -> List (Html (Msg HMMsg))
view m =
    let
        model = { screensize = m.screensize
                , status = m.status
                , count = m.count
                , showingCollections = m.showingCollections
                , collections = m.collections
                , words = m.words
                , gameModel = case m.gameModel of
                    Nothing -> initModel
                    Just gameModel -> gameModel
                }
    in
    [ navBar (Just "Hangman") (appMenu model) [action "Back" (SelectGame 0)]
    , gameStage model
    ]


appMenu : Model HMModel -> List (Action (Msg HMMsg))
appMenu model =
    if 0 == ( model.gameModel.words |> List.length )
    then [ action "Collections" ( ShowCollection True ) ]
    else
        [ action "Collections" ( ShowCollection True ) ] ++
        ( case model.status of
            IN_GAME -> [ action "Pause" PauseGame ]
            PAUSED -> [ action "Resume" ResumeGame ]
            INIT -> [ action "Start" StartGame ]
            _ -> []
        )

gameStage : Model HMModel -> Html (Msg HMMsg)
gameStage model =
    let
        gameModel = model.gameModel
        currentWord = model.gameModel.hiddenWord
        showingResult = model.gameModel.showingResult
        keydownDecoder = (Json.string |> Json.andThen (String.toLower >> Json.succeed))
        isLost = ( List.length gameModel.gallowStatus ) == ( Array.length executionSequence )
        showingNext = currentWord
            |> Maybe.map (\word ->
                0 == ( word.displayedIndices
                |> List.filter ((==)False)
                |> List.length ) || isLost
            )
            |> Maybe.withDefault False
    in
    div [ class "container-fluid container-lg"
        , id "stage"
        , tabindex 1
        , on "keydown" (Json.map (\maybeEnteredKey ->
            maybeEnteredKey
            |> Maybe.andThen (\enteredKey ->
                currentWord
                |> Maybe.map (\word ->
                    if (String.contains enteredKey word.text)
                        then (GameMsg << PickCharacter) enteredKey
                        else (GameMsg PickWrong)
                    )
                )
            |> Maybe.withDefault NoOp
            ) (Json.map2
                (\filteringKey key -> if String.length filteringKey > 1 then Nothing else Just key)
                (Json.field "key" Json.string)
                (Json.field "key" keydownDecoder)
            ))
        ]
        [ div [ class "row" ]
            ( if model.status == GAMEOVER
                then
                    [ div [ class "col" ]
                        [ gameoverElement StartGame
                        , reportElement gameModel.answers
                        ]
                    ]
                else [ currentWord
                    |> Maybe.map (\word ->
                              div [ class "col-8" ]
                                  [ hiddenBoxes word
                                  , div []
                                      [ p [ class "text-center display-2" ] [ text word.hint ]
                                      ]
                                  ]
                              )
                          |> Maybe.withDefault empty
                    , div [ class "col-4" ]
                        [ gallowElement gameModel.gallowStatus
                        ]
                    ]
            )
        , if not showingResult
            then
                currentWord
                |> Maybe.map (\word ->
                    div [ class "row" ]
                        [ keyboardElement word.text ]
                    )
                |> Maybe.withDefault empty
            else empty
        , if showingNext
            then ( div [ class "col-8 text-center"]
                [ button
                    [ class "btn btn-lg btn-primary"
                    , onClick (GameMsg NextWord)
                    ] [ text "Next"]
                ]
            )
            else empty
        ]

hiddenBoxes : HiddenWord -> Html (Msg HMMsg)
hiddenBoxes hiddenWord =
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
    in
    div [ class "hidden-boxes mt-5" ]
        [ div [ class "input-group hangman-hidden-word mx-auto" ]
            ( hiddenWord.displayedIndices
            |> List.indexedMap (\index displayed ->
                span
                    [ disabled (fnDisabled index |> Maybe.withDefault True)
                    , maxlength 1
                    , class "form-control form-control-lg me-1"
                    ] [ text ( fnVisibleChatAt index |> Maybe.withDefault "_" ) ]
                )
            )
        ]


gallowElement : List GallowStatus -> Html msg
gallowElement stati =
    let
        gallowClass = if 0 < ( stati |> List.filter ((==) Dead) |> List.length )
                then "gallow dead" else "gallow"
    in
    div [ class gallowClass ]
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

keyboardElement : String -> Html (Msg HMMsg)
keyboardElement wordText =
    let
        onKey key =
            if String.contains key wordText
                then (GameMsg << PickCharacter) key
                else GameMsg PickWrong
        row1Keys = String.split "" "qwertzuiopü"
        row2Keys = String.split "" "asdfghjklöä"
        row3Keys = String.split "" "yxcvbnm "

        btn key = button [ class "btn btn-sm btn-outline-secondary mx-1 my-1", onClick (onKey key) ] [ text key ]
    in
    div [ class "col-8 keyboard" ]
        [ div [ class "key-row d-flex justify-content-center" ]
            ( row1Keys |> List.map btn
            )
        , div [ class "key-row d-flex justify-content-center" ]
            ( row2Keys |> List.map btn
            )
        , div [ class "key-row d-flex justify-content-center" ]
            ( row3Keys |> List.map btn
            )
        ]
