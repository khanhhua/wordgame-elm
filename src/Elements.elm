module Elements exposing (..)

import Common exposing (Model, dumpster_inner_width)
import Data exposing (Answer, Word)
import Json.Decode
import Svg as S
import Svg.Attributes as SA

import Html exposing (Html, a, button, div, li, nav, text, ul)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Svg.Events as SE

type Action msg = Action String msg

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


stage : (Word -> msg) -> (Word -> msg) -> ( String, String ) -> Model -> Html msg
stage onAnimationComplete onSelectWord ( width, height ) model =
    S.svg
        [ SA.width width
        , SA.height height
        , SA.viewBox ( "0 0 " ++ width ++ " " ++ height )
        , style "background-color" "#eee"
        ]
        ( [ dumster ]
        ++
        ( model.stagedWords |> List.map ( wordSprite onAnimationComplete onSelectWord True ) )
        )


dumster : S.Svg msg
dumster =
    S.rect
        [ SA.x "10"
        , SA.y "10"
        , SA.width dumpster_inner_width
        , SA.height "500"
        , SA.rx "5"
        , SA.ry "5"
        , SA.fill "#ddd"
        ]
        []

wordSprite : (Word -> msg) -> (Word -> msg) -> Bool -> Word -> S.Svg msg
wordSprite onAnimationComplete onSelectWord animating word =
    let
        (x, y) = word.position
        class = SA.class ( if animating then "word-sprite animation-tetrix" else "word-sprite" )
    in
     S.text_
         [ SA.id word.text
         , SA.x ( String.fromInt x )
         , SA.y ( String.fromInt y )
         , SA.textAnchor "middle"
         , SA.fill "black"
         , class
         , SE.onClick ( onSelectWord word )
         , SE.on "animationend" ( Json.Decode.succeed ( onAnimationComplete word ) )
         ]
         [ S.text word.text
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
