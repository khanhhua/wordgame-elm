module GenderRace exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Process
import Random
import Task
import Time

import Common exposing (..)
import Elements exposing (Action, action, gameoverElement, navBar, onAnimationEnd, px, pxI, reportElement, stageSize)

type GRMsg
    = ApplyRandomness (List Word)
    | LoadWords ( List Word )
    | WordAnimationComplete Word
    | SelectAnswer Word String
    | Tick Time.Posix
    | GrNoOp


type alias GRModel =
    { count : Int
    , words : List Word
    , stagedWords : List Word
    , answers : List Answer
    }


initModel : GRModel
initModel =
    { count = 0
    , words = []
    , stagedWords = []
    , answers = []
    }


update : Msg GRMsg -> GRModel -> ( GRModel, Cmd (Msg GRMsg))
update msg model =
    case msg of
        GameMsg (LoadWords words) ->
            ( { model | words = words |> filterWords [MAS, FEM, NEU] }, Cmd.none )
        GameMsg (ApplyRandomness words) ->
            ( { model
                | words = words
                }
            , Cmd.none
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
                    then model.stagedWords |> List.map ( \item -> { item | expired = False })
                    else model.words
            in
            ( { model
                | words = words
                , stagedWords = stagedWords
                , answers = []
                }, Random.generate (GameMsg << ApplyRandomness) randomizer )
        GameMsg (WordAnimationComplete key) ->
            let
                count = model.count - 1
                --status = if 0 == count then GAMEOVER else model.status
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
                } , Cmd.none)
        GameMsg (SelectAnswer word answerText) ->
            let
                answers =
                    if ( List.member MAS word.tags && answerText == "DER" )
                        || ( List.member FEM word.tags && answerText == "DIE" )
                        || ( List.member NEU word.tags && answerText == "DAS" )
                    then
                         ( Answer word.text word.tags True ) :: model.answers
                    else
                         ( Answer word.text word.tags False ) :: model.answers
                stagedWords = model.stagedWords
                    |> List.map (\word_ ->
                        if word_ == word then
                            { word_ | expired = True }
                        else
                            word_
                    )
                    |> updatePositions
                --status = if
                --        0 == ( stagedWords |> List.filter ( .expired >> not ) |> List.length )
                --        && 0 == ( model.words |> List.length )
                --    then GAMEOVER
                --    else model.status
            in
            ( { model
                | answers = answers
                , stagedWords = stagedWords
                --, status = status
                }, Cmd.none )
        GameMsg (Tick _) ->
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
                gameCmd = if 0 == ( words |> List.length ) || overflow
                    then (
                        Process.sleep (1/release_frequency * 990)
                        |> Task.andThen ((always << Task.succeed) CompleteGame)
                        |> Task.perform identity
                    )
                    else Cmd.none
            in
            ({ model
                | words = words
                , stagedWords = stagedWords
                } , gameCmd )
        _ -> ( model, Cmd.none )


subscriptions : Model GRModel -> Sub GRMsg
subscriptions model =
    if model.status == IN_GAME then
        Sub.batch [ Time.every (1/release_frequency * 1000) Tick ]
    else
        Sub.none


view : Model (Maybe GRModel) -> List (Html (Msg GRMsg))
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
    [ navBar (Just "Gender Race") (appMenu model) [action "Back" (SelectGame 0)]
    , gameStage (stageSize model.screensize) model
    ]

appMenu : Model GRModel -> List (Action (Msg GRMsg))
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


gameStage : (Int, Int) -> Model GRModel -> Html (Msg GRMsg)
gameStage stageSize model =
    div [ class "container container-md-fluid" ]
        [ div [ class "row" ]
            ( if model.status == GAMEOVER
            then [ gameoverElement StartGame
                , reportElement model.gameModel.answers
                ]
            else
                [ model.gameModel |> stage
                    (WordAnimationComplete >> GameMsg)
                    (\a b -> (SelectAnswer a b) |> GameMsg)
                    stageSize
                ]
            )
        ]


stage : (Word -> msg) -> (Word -> String -> msg) -> ( Int, Int ) -> GRModel -> Html msg
stage onAnimationComplete onAnswer ( width, height ) model =
    div
        [ class "stage"
        , style "width" ( pxI width )
        , style "height" ( pxI height )
        , class "position-relative mt-1 mx-auto"
        ]
        ( model.stagedWords
            |> List.map ( \word -> wordSprite onAnimationComplete onAnswer True word )
        )


wordSprite : (Word -> msg) -> (Word -> String -> msg) -> Bool -> Word -> Html msg
wordSprite onAnimationComplete onAnswer animating word =
    let
        (x, y) = word.position
        class_ = class ( if animating
            then "word-sprite animation-tetrix"
            else "word-sprite" )
        onSelectAnswer = onAnswer word
    in
    if word.expired then
        text ""
    else
        div
            [ id word.text
            , style "left" ( px (toFloat x) )
            , style "top" ( px (toFloat y ) )
            , style "color" "black"
            , class_
            , onAnimationEnd ( onAnimationComplete word )
            ]
                [ div [ class "badge bg-light text-dark rounded-pill"]
                    [ text word.text ]
                , answerBar onSelectAnswer
                ]


answerBar : ( String -> msg ) -> Html msg
answerBar onSelectAnswer =
    div [ class "d-flex mt-3" ]
        [ button
            [ class "btn btn-light btn-lg bg-masculine text-light mx-1 fs-4"
            , style "width" "33.33%"
            , style "height" "2.5em"
            , onClick ( onSelectAnswer "DER" )
            --, onTouchStart ( onSelectAnswer "DER" )
            ] [ text "DER" ]
        , button
            [ class "btn btn-light btn-lg bg-feminine text-light mx-1 fs-4"
            , style "width" "33.33%"
            , style "height" "2.5em"
            , onClick ( onSelectAnswer "DIE" )
            --, onTouchStart ( onSelectAnswer "DIE" )
            ] [ text "DIE" ]
        , button
            [ class "btn btn-light btn-lg bg-neutrum text-light mx-1 fs-4"
            , style "width" "33.33%"
            , style "height" "2.5em"
            , onClick ( onSelectAnswer "DAS" )
            --, onTouchStart ( onSelectAnswer "DAS" )
            ] [ text "DAS" ]
        ]
