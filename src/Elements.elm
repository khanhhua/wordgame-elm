module Elements exposing (..)

import Common exposing (Answer, Collection, Model, Word, genderToString, getHi, getWi)
import Json.Decode

import Html exposing (Attribute, Html, a, button, div, h3, li, nav, p, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (on, onClick)

type Action a = Action String a


chooseGameTypePage : ( Int -> msg ) -> List ( Html msg )
chooseGameTypePage onSelectGame =
    [ navBar Nothing [] []
    , div [ class "row" ]
        [ div [ class "col-6 display-6 mt-3 mx-auto" ]
            [ div [ class "choose-game" ]
                  [ div [ class "list-group" ]
                      [ a [ class "list-group-item list-group-item-action"
                          , onClick ( onSelectGame 1 )
                          ] [ text "Gender Race" ]
                      , a [ class "list-group-item list-group-item-action"
                          , onClick ( onSelectGame 2 )
                          ] [ text "Hangman" ]
                      ]
                  ]
            ]
        ]
    ]


px : Float -> String
px l = ( String.fromFloat l ) ++ "px"

pxI : Int -> String
pxI l = ( String.fromInt l ) ++ "px"

action : String -> msg -> Action msg
action label msg =
    Action label msg

navBar : Maybe String -> List ( Action msg ) -> List ( Action msg ) -> Html msg
navBar gameTitle leftActions rightActions =
    nav [ class "navbar navbar-expand navbar-light bg-light sticky-top" ]
        [ div [ class "container-fluid" ]
            [ ul [ class "col navbar-nav me-auto" ]
                ( leftActions
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
            , a [class "col navbar-brand mx-auto fs-2 text-center"]
                [ text (gameTitle
                    |> Maybe.map (\title -> "WordGame: " ++ title)
                    |> Maybe.withDefault "WordGame")
                ]
            , div [ class "col navbar-nav mr-0 flex-row-reverse" ]
                ( rightActions
                    |> List.map (\item ->
                        let
                            ( label, msg ) = case item of
                                Action label_ msg_ -> ( label_, msg_ )
                        in
                        button [ class "btn btn-outline-dark", onClick msg ] [ text label ]
                    )
                )
            ]
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
