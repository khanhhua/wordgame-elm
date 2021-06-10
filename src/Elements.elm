module Elements exposing (..)

import Common exposing (Model, dumpster_inner_width)
import Data exposing (Answer, Word)
import Json.Decode
--import Svg as S
--import Svg.Attributes as SA

import Html exposing (Html, a, button, div, li, nav, span, text, ul)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (on, onClick)
--import Svg.Events as SE

type Action msg = Action String msg


px : Int -> String
px l = ( String.fromInt l ) ++ "px"

action : String -> msg -> Action msg
action label msg =
    Action label msg

navBar : List ( Action msg ) -> Html msg
navBar actions =
    nav [ class "navbar navbar-expand navbar-light bg-light" ]
        [ div [ class "container-fluid" ]
            [ a [class "navbar-brand"] [ text "WordGame" ]
            , ul [ class "navbar-nav me-auto" ]
                ( actions
                    |> List.map ( \item ->
                        let
                            ( label, msg ) = case item of
                                Action label_ msg_ -> ( label_, msg_ )
                        in
                        li [ class "nav-item" ]
                            [ a
                                [ onClick msg
                                , class "btn btn-light" ]
                                [ text label ]
                            ]
                    )
                )
            ]
        ]


stage : (Word -> msg) -> (Word -> String -> msg) -> ( Int, Int ) -> Model -> Html msg
stage onAnimationComplete onAnswer ( width, height ) model =
    div
        [ class "stage"
        , style "width" ( px width )
        , style "height" ( px height )
        , class "position-relative mt-1"
        , style "background-color" "#eee"
        , style "margin-left" "-10px"
        ]
        ( [ dumster ( height - 20 ) ]
        ++
        ( model.stagedWords
            |> List.map ( \word -> wordSprite onAnimationComplete onAnswer True word ) )
        )


dumster : Int -> Html msg
dumster height =
    div
        [ class "position-absolute"
        , style "left" "10px"
        , style "top" "10px"
        , style "width" ( px dumpster_inner_width )
        , style "height" ( px height )
        , style "border-radius" "5px"
        , style "background-color" "#ddd"
        ]
        []

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
            , style "left" ( px x )
            , style "top" ( px y )
            , style "color" "black"
            , class_
            , on "animationend" ( Json.Decode.succeed ( onAnimationComplete word ) )
            ]
                [ div [ class "badge bg-light text-dark rounded-pill"]
                    [ text word.text ]
                , answerBar onSelectAnswer
                ]


answerBar : ( String -> msg ) -> Html msg
answerBar onSelectAnswer =
    div [ class "d-flex mt-3" ]
        [ button
            [ class "btn btn-light btn-lg bg-masculine text-light mx-1"
            , style "width" "33.33%"
            , style "height" "4.5em"
            , onClick ( onSelectAnswer "DER" )
            ] [ text "DER" ]
        , button
            [ class "btn btn-light btn-lg bg-feminine text-light mx-1"
            , style "width" "33.33%"
            , style "height" "4.5em"
            , onClick ( onSelectAnswer "DIE" )
            ] [ text "DIE" ]
        , button
            [ class "btn btn-light btn-lg bg-neutrum text-light mx-1"
            , style "width" "33.33%"
            , style "height" "4.5em"
            , onClick ( onSelectAnswer "DAS" )
            ] [ text "DAS" ]
        ]

stats : List Answer -> Html msg
stats answers =
    let
        correctCount = answers |> List.foldl ( \item acc -> if item.correct then acc + 1 else acc ) 0
        wrongCount = answers |> List.foldl ( \item acc -> if not item.correct then acc + 1 else acc ) 0
    in
    ul [ class "list-group list-group-horizontal" ]
        [ li [ class "list-group-item w-50" ]
            [ text ( "Correct: " ++ String.fromInt correctCount )
            ]
        , li [ class "list-group-item w-50" ]
            [ text ( "Wrong: " ++ String.fromInt wrongCount )
            ]
        ]
