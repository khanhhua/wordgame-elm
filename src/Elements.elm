module Elements exposing (..)

import Common exposing (Model, dumpster_inner_width)
import Data exposing (Answer, Word)
import Json.Decode

import Html exposing (Attribute, Html, a, button, div, li, nav, span, text, ul)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (on, onClick)

type Action msg = Action String msg


px : Int -> String
px l = ( String.fromInt l ) ++ "px"

action : String -> msg -> Action msg
action label msg =
    Action label msg

navBar : List ( Action msg ) -> Model -> Html msg
navBar actions model =
    nav [ class "navbar navbar-expand navbar-light bg-light" ]
        [ div [ class "container-fluid" ]
            [ a [class "navbar-brand fs-2"] [ text "WordGame" ]
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
                                , class "btn btn-light fs-4" ]
                                [ text label ]
                            ]
                    )
                )
            , div [ class "navbar-nav mx-auto" ] [ stats model.answers ]
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
            , style "left" ( px x )
            , style "top" ( px y )
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
            , onTouchStart ( onSelectAnswer "DER" )
            ] [ text "DER" ]
        , button
            [ class "btn btn-light btn-lg bg-feminine text-light mx-1 fs-4"
            , style "width" "33.33%"
            , style "height" "2.5em"
            , onClick ( onSelectAnswer "DIE" )
            , onTouchStart ( onSelectAnswer "DIE" )
            ] [ text "DIE" ]
        , button
            [ class "btn btn-light btn-lg bg-neutrum text-light mx-1 fs-4"
            , style "width" "33.33%"
            , style "height" "2.5em"
            , onClick ( onSelectAnswer "DAS" )
            , onTouchStart ( onSelectAnswer "DAS" )
            ] [ text "DAS" ]
        ]

stats : List Answer -> Html msg
stats answers =
    let
        correctCount = answers |> List.foldl ( \item acc -> if item.correct then acc + 1 else acc ) 0
        wrongCount = answers |> List.foldl ( \item acc -> if not item.correct then acc + 1 else acc ) 0
    in
    ul [ class "list-group list-group-horizontal text-nowrap" ]
        [ li [ class "list-group-item w-50 fs-5" ]
            [ text ( "Correct: " ++ String.fromInt correctCount )
            ]
        , li [ class "list-group-item w-50 fs-5" ]
            [ text ( "Wrong: " ++ String.fromInt wrongCount )
            ]
        ]


onTouchStart : msg -> Attribute msg
onTouchStart msg =
    on "touchstart" ( Json.Decode.succeed msg )

onAnimationEnd : msg -> Attribute msg
onAnimationEnd msg =
    on "animationend" ( Json.Decode.succeed msg )
