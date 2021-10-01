module Main exposing (..)

import Browser exposing (Document)
import Browser.Dom
import Browser.Navigation as Navigation

import GenderRace as GR exposing (GRMsg(..))
import Hangman as HM exposing (HMMsg(..))

import Html exposing (Html, button, div, text)
import Task
import Url

import Common exposing (..)
import Data exposing (..)
import Elements exposing (Action(..), action, collectionListElement, navBar, stageSize)

type GameMsg
    = GR GRMsg
    | HM HMMsg
type alias AppMsg = Msg GameMsg

type GameModel
    = GameModelGR GR.GRModel
    | GameModelHM HM.HMModel

main : Program () (Model GameModel) AppMsg
main =
  Browser.application
    { init = init
    , view = view
    , subscriptions = subscriptions
    , update = update
    , onUrlChange = (\_ -> NoOp)
    , onUrlRequest = (\_ -> NoOp)
    }


init : () -> Url.Url -> Navigation.Key -> ( Model GameModel, Cmd AppMsg )
init () url key =
    ( initModel ( GameModelGR GR.initModel )
    , Cmd.batch
        [ Task.perform SetScreenSize Browser.Dom.getViewport
        , loadCollectionsMetadata GotCollections
        ]
    )


update : AppMsg -> Model GameModel -> ( Model GameModel, Cmd AppMsg )
update msg model =
    case msg of
        SetScreenSize screen ->
            let
                size = ( screen.viewport.width, screen.viewport.height )
            in
            ( { model | screensize = size } , Cmd.none )
        GotCollections collections ->
            ( { model | collections = collections |> List.append model.collections } , Cmd.none )
        ShowCollection bool ->
            ( { model | showingCollections = bool } , Cmd.none )
        SelectFile fileName ->
            let
                updatedModel = resetGame model
            in
            ( { updatedModel
                | showingCollections = False
                }
            , loadFileByName GotWordList fileName )
        GotWordList words ->
            ( { model
                | count = words |> List.length
                , words = words
                }, Task.perform (\words_ ->
                    case model.game of
                        GameHangMan -> GameMsg (HM (HM.LoadWords words_))
                        GameGenderRace -> GameMsg (GR (GR.LoadWords words_))
                ) (Task.succeed words)
            )
        GameMsg (GR msg_) ->
            let
                ( updatedModel, cmd ) = case model.gameModel of
                    GameModelGR gameModel ->
                        let
                            ( gameModel_, grCmd ) = GR.update msg_ gameModel
                        in
                        ( { model | gameModel = GameModelGR gameModel_ }
                        , Cmd.map (GR >> GameMsg) grCmd
                        )
                    _ -> ( model, Cmd.none )
            in ( updatedModel,  cmd )
        GameMsg (HM msg_) ->
            let
                ( updatedModel, cmd ) = case model.gameModel of
                    GameModelHM gameModel ->
                        let
                            ( gameModel_, hmCmd ) = HM.update msg_ gameModel
                        in
                        ( { model | gameModel = GameModelHM gameModel_ }
                        , Cmd.map (HM >> GameMsg) hmCmd
                        )
                    _ -> ( model, Cmd.none )
            in ( updatedModel,  cmd )
        _ -> ( model, Cmd.none )


subscriptions : Model GameModel -> Sub AppMsg
subscriptions model =
    case model.game of
        GameGenderRace ->
            case model.gameModel of
                GameModelGR gameModel_ -> Sub.map (GR >> GameMsg) (GR.subscriptions gameModel_)
                _ -> Sub.none
        GameHangMan ->
            case model.gameModel of
                GameModelHM gameModel_ -> Sub.map (HM >> GameMsg) (HM.subscriptions gameModel_)
                _ -> Sub.none


view : Model GameModel -> Document AppMsg
view model =
    let
        stageSize_ = stageSize model.screensize
        renderAppMenu =
            case model.game of
                GameGenderRace ->
                    case model.gameModel of
                        GameModelGR model_ ->
                            model_
                            |> GR.appMenu >> List.map (\(Action label msg) -> Action label ((GameMsg << GR) msg))
                        _ -> []
                GameHangMan ->
                    case model.gameModel of
                        GameModelHM model_ ->
                            model_
                            |> HM.appMenu >> List.map (\(Action label msg) -> Action label ((GameMsg << HM) msg))
                        _ -> []

        renderStage =
            case model.game of
                GameGenderRace ->
                    case model.gameModel of
                        GameModelGR model_ ->
                            model_ |> GR.gameStage model.screensize >> Html.map (GameMsg << GR)
                        _ -> empty
                GameHangMan ->
                    case model.gameModel of
                        GameModelHM model_ ->
                            model_ |> HM.gameStage >> Html.map (GameMsg << HM)
                        _ -> empty

        getAnswers =
            case model.gameModel of
                GameModelGR model_ -> model_.answers
                GameModelHM model_ -> model_.answers
    in
    { title = "WordGame - ELM 2021"
    , body =
        [ getAnswers
            |> navBar ( [ action "Collections" ( ShowCollection True ) ] ++ renderAppMenu )
        , collectionListElement SelectFile ( ShowCollection False ) model.showingCollections model.collections
        , renderStage
        ]
    }
