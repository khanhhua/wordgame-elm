module Elements exposing (..)

import Common exposing (Answer, Collection, Model, Word, genderToString, getHi, getWi)
import Json.Decode

import Html exposing (Attribute, Html, a, button, div, h3, li, nav, p, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (on, onClick)

type Action a = Action String a


px : Int -> String
px l = ( String.fromInt l ) ++ "px"

action : String -> msg -> Action msg
action label msg =
    Action label msg

navBar : List ( Action msg ) -> Model a -> Html msg
navBar actions model =
    nav [ class "navbar navbar-expand navbar-light bg-light sticky-top" ]
        [ div [ class "container-fluid" ]
            [ ul [ class "col navbar-nav me-auto" ]
                ( actions
                    |> List.map ( \item ->
                        let
                            ( label, msg ) = case item of
                                Action label_ msg_ -> ( label_, msg_ )
                        in
                        li [ class "nav-item" ]
                            [ a
                                [ onClick msg
                                , class "btn btn-outline-dark mx-1 fs-4" ]
                                [ text label ]
                            ]
                    )
                )
            , a [class "col navbar-brand mx-auto fs-2 text-center"] [ text "WordGame" ]
            , div [ class "col navbar-nav mr-0" ] [ stats model.answers ]
            ]
        ]


stage : (Word -> msg) -> (Word -> String -> msg) -> ( Int, Int ) -> Model a -> Html msg
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


gameoverElement : msg -> Html msg
gameoverElement onStartGame =
    div [ class "display-1 text-center fw-bold mt-50p" ]
        [ p [] [ text "GAME OVER!" ]
        , button
            [ onClick onStartGame
            , class "btn btn-primary rounded-pill mt-5 px-sm-5 display-1"
            ] [ text "RESTART!" ]
        ]

reportElement : List Answer -> Html msg
reportElement answers =
    if 0 == ( answers |> List.length )
    then ( text "" )
    else div [ class "col mx-auto mt-4" ]
        [ table [ class "table display-6" ]
            [ thead []
                [ tr []
                    [ th [] [ text "#" ]
                    , th [] [ text "Word" ]
                    , th [] [ text "Gender" ]
                    , th [] [ text "Correct" ]
                    ]
                ]
            , tbody [ class "font-monospace" ]
                ( answers |> List.indexedMap ( \index answer ->
                    tr []
                        [ td [] [ text ( index |> ((+) 1) >> String.fromInt ) ]
                        , td [] [ text answer.text ]
                        , td [] [ text ( answer.gender |> genderToString ) ]
                        , td [] [ text ( if answer.correct then "x" else "o" ) ]
                        ]
                ) )
            ]
        ]

collectionListElement : ( String -> msg ) -> msg -> Bool -> List Collection -> Html msg
collectionListElement onSelectFile onClose shown collections =
    div [ class (
        "offcanvas offcanvas-start collection-list"
        ++ ( if shown then " show visible" else "" ) )
        ]
        [ div [ class "d-flex justify-content-between" ]
            [ h3 [ class "p-2" ] [ text "Collections" ]
            , button [ class "btn btn-sm btn-dark m-1", onClick onClose ] [ text "Close" ]
            ]
        , ul [ class "list-group" ]
            ( collections |> List.map ( \collection ->
                li [ onClick ( collection.packageId ++ "/" ++ collection.file |> onSelectFile  )
                    , class "list-group-item display-6"
                    ] [ text collection.name ]
            ) )
        ]

-- FUNCTIONS

stageSize : ( Float, Float ) -> ( Int, Int )
stageSize screensize =
    let
        w = getWi screensize
        h = getHi screensize
    in
    ( w - 200, h - 200 )


onTouchStart : msg -> Attribute msg
onTouchStart msg =
    on "touchstart" ( Json.Decode.succeed msg )

onAnimationEnd : msg -> Attribute msg
onAnimationEnd msg =
    on "animationend" ( Json.Decode.succeed msg )
