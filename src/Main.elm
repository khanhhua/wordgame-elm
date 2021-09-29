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
import Elements exposing (Action(..), action, collectionListElement, gameoverElement, navBar, reportElement, stage, stageSize)

type GameMsg
    = GR GRMsg
    | HM HMMsg
type alias AppMsg = Msg GameMsg

type GameModel
    = GameModelGR {}
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
    ( initModel ( GameModelHM HM.initModel )
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
                    GameMsg (HM (HM.LoadWords words_))
                ) (Task.succeed words)
            )
        GameMsg (GR msg_) ->
            let
                ( updatedModel, grCmd ) = GR.update msg_ model
            in ( updatedModel, Cmd.map (GR >> GameMsg) grCmd )
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


subscriptions : Model a -> Sub AppMsg
subscriptions model =
    case model.game of
        GameGenderRace -> Sub.map (GR >> GameMsg) (GR.subscriptions model)
        GameHangMan -> Sub.map (HM >> GameMsg) (HM.subscriptions model)


view : Model GameModel -> Document AppMsg
view model =
    let
        stageSize_ = stageSize model.screensize
        renderAppMenu =
            case model.game of
                GameGenderRace ->
                    ((GR.appMenu model) |> List.map (\(Action label msg) -> Action label ((GameMsg << GR) msg)))
                GameHangMan ->
                    let
                        hmModel = case model.gameModel of
                            GameModelHM model_ -> Just model_
                            _ -> Nothing
                    in
                    hmModel
                        |> Maybe.map (HM.appMenu >> List.map (\(Action label msg) -> Action label ((GameMsg << HM) msg)))
                        |> Maybe.withDefault []
        renderStage =
            case model.game of
                GameGenderRace -> GR.gameStage model |> Html.map (GameMsg << GR)
                GameHangMan ->
                    let
                        hmModel = case model.gameModel of
                            GameModelHM model_ -> Just model_
                            _ -> Nothing
                    in
                    hmModel
                        |> Maybe.map (HM.gameStage >> Html.map (GameMsg << HM))
                        |> Maybe.withDefault empty

        ( colW, _ ) = stageSize_
    in
    { title = "WordGame - ELM 2021"
    , body =
        [ model |> navBar
            ( [ action "Collections" ( ShowCollection True ) ] ++ renderAppMenu )
        , collectionListElement SelectFile ( ShowCollection False ) model.showingCollections model.collections
        , renderStage
        ]
    }
