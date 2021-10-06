module GenderRace exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Random
import Time

import Common exposing (..)
import Elements exposing (Action, action, gameoverElement, onAnimationEnd, px, reportElement, stageSize)

type GRMsg
    = ApplyRandomness (List Word)
    | LoadWords ( List Word )
    | StartGame
    | PauseGame
    | ResumeGame
    | WordAnimationComplete Word
    | SelectAnswer Word String
    | Tick Time.Posix
    | GrNoOp


type alias GRModel =
    { status : GameStatus
    , count : Int
    , words : List Word
    , stagedWords : List Word
    , answers : List Answer
    }


initModel : GRModel
initModel =
    { status = MENU
    , count = 0
    , words = []
    , stagedWords = []
    , answers = []
    }


update : GRMsg -> GRModel -> ( GRModel, Cmd GRMsg)
update msg model =
    case msg of
        LoadWords words ->
            ( { model | words = words }, Cmd.none )
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


subscriptions : GRModel -> Sub GRMsg
subscriptions model =
    if model.status == IN_GAME then
        Sub.batch [ Time.every (1/release_frequency * 1000) Tick ]
    else
        Sub.none


appMenu : GRModel -> List (Action GRMsg)
appMenu model =
    if 0 == ( model.words |> List.length ) then []
    else
        ( case model.status of
            IN_GAME -> [ action "Pause" PauseGame ]
            PAUSED -> [ action "Resume" ResumeGame ]
            MENU -> [ action "Start" StartGame ]
            _ -> []
        )


gameStage : (Float, Float) -> GRModel -> Html GRMsg
gameStage stageSize model =
    div [ class "container container-md-fluid" ]
        [ div [ class "row" ]
            ( if model.status == GAMEOVER
            then [ gameoverElement StartGame
                , reportElement model.answers
                ]
            else
                [ model |> stage
                    WordAnimationComplete
                    SelectAnswer
                    stageSize
                ]
            )
        ]


stage : (Word -> msg) -> (Word -> String -> msg) -> ( Float, Float ) -> GRModel -> Html msg
stage onAnimationComplete onAnswer ( width, height ) model =
    div
        [ class "stage"
        , style "width" ( px width )
        , style "height" ( px height )
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
