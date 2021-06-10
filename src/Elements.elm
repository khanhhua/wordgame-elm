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
    nav [ class "navbar navbar-expand-lg navbar-light bg-light" ]
        [ div [ class "container-fluid" ]
            [ a [class "navbar-brand"] [ text "WordGame" ]
            , ul [ class "navbar-nav mr-0" ]
                ( actions
                    |> List.map ( \item ->
                        let
                            ( label, msg ) = case item of
                                Action label_ msg_ -> ( label_, msg_ )
                        in
                        button
                            [ onClick msg
                            , class "btn btn-light" ]
                            [ text label ]
                    )
                )
            ]
        ]


stage : (Word -> msg) -> (Word -> msg) -> ( Int, Int ) -> Model -> Html msg
stage onAnimationComplete onSelectWord ( width, height ) model =
    let
        isHighlighted : Word -> Bool
        isHighlighted = case model.selectedWord of
            Nothing -> ( \_ -> False )
            Just key -> ( \word -> word == key )
    in
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
            |> List.map ( \word -> wordSprite onAnimationComplete onSelectWord ( isHighlighted word ) True  word ) )
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

wordSprite : (Word -> msg) -> (Word -> msg) -> Bool -> Bool -> Word -> Html msg
wordSprite onAnimationComplete onSelectWord highlighted animating word =
    let
        (x, y) = word.position
        class_ = class ( if animating then "word-sprite animation-tetrix" else "word-sprite" )
    in
     span
         [ id word.text
         , style "left" ( px x )
         , style "top" ( px y )
         , style "color" ( if highlighted then "red" else "black" )
         , class_
         , onClick ( onSelectWord word )
         , on "animationend" ( Json.Decode.succeed ( onAnimationComplete word ) )
         ]
         [ text word.text
         ]


answerBar : ( String -> msg ) -> Html msg
answerBar onSelectAnswer =
    div [ class "d-flex mt-3" ]
        [ button
            [ class "btn btn-outline-secondary mx-1"
            , style "width" "33.33%"
            , onClick ( onSelectAnswer "DER" )
            ] [ text "DER" ]
        , button
            [ class "btn btn-outline-secondary mx-1"
            , style "width" "33.33%"
            , onClick ( onSelectAnswer "DIE" )
            ] [ text "DIE" ]
        , button
            [ class "btn btn-outline-secondary mx-1"
            , style "width" "33.33%"
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
