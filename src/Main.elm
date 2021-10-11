module Main exposing (..)

import Browser exposing (Document)
import Browser.Dom
import Browser.Navigation as Navigation

import GenderRace as GR exposing (GRMsg(..))
import Hangman as HM exposing (HMMsg(..))

import Html exposing (Html, a, div, text)
import Task
import Url

import Common exposing (..)
import Data exposing (..)
import Elements exposing (Action(..), collectionListElement, chooseGameTypePage)

type GameMsg
    = GR GRMsg
    | HM HMMsg
type alias AppMsg = Msg GameMsg

type GameModel
    = GameModelGR GR.GRModel
    | GameModelHM HM.HMModel
    | NoGame

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
    ( initModel NoGame
    , Cmd.batch
        [ Task.perform SetScreenSize Browser.Dom.getViewport
        , loadCollectionsMetadata GotCollections
        ]
    )


update : AppMsg -> Model GameModel -> ( Model GameModel, Cmd AppMsg )
update msg model =
    let
        gameResponse = case model.gameModel of
            GameModelGR gameModel ->
                let
                    gameResponse_ = case msg of
                        StartGame -> Just (GR.update StartGame gameModel)
                        _ -> Nothing
                in
                gameResponse_
                |> Maybe.map (\(gameModel_, grCmd) ->
                    ( { model
                        | gameModel = GameModelGR gameModel_
                        , status = IN_GAME
                        }
                    , Cmd.map (\a ->
                        case a of
                            GameMsg a_ -> GameMsg (GR a_)
                            _ -> NoOp
                        ) grCmd
                    ))
            GameModelHM gameModel ->
                let
                    gameResponse_ = case msg of
                        StartGame -> Just (HM.update StartGame gameModel)
                        _ -> Nothing
                in
                gameResponse_
                |> Maybe.map (\(gameModel_, hmCmd) ->
                    ( { model
                        | gameModel = GameModelHM gameModel_
                        , status = IN_GAME
                        }
                    , Cmd.map (\a -> case a of
                            GameMsg a_ -> GameMsg (HM a_)
                            _ -> NoOp
                        ) hmCmd
                    ))
            _ -> Nothing
    in
    gameResponse
    |> Maybe.withDefault
        ( case msg of
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
                    , status = INIT
                    }
                , loadFileByName GotWordList fileName )
            GotWordList words ->
                ( { model
                    | count = words |> List.length
                    , words = words
                    },
                    if model.gameModel == NoGame
                    then Cmd.none
                    else
                        Task.perform (\words_ ->
                           case model.gameModel of
                               NoGame -> NoOp
                               GameModelHM _ -> GameMsg (HM (HM.LoadWords words_))
                               GameModelGR _ -> GameMsg (GR (GR.LoadWords words_))
                        ) (Task.succeed words)
                )
            SelectGame gameId ->
                case gameId of
                    1 ->
                        let
                            gm = GR.initModel
                        in
                        ( { model
                            | gameModel = GameModelGR { gm | words = model.words }
                            , status = INIT
                            }
                        , Cmd.none )
                    2 ->
                        let
                            gm = HM.initModel
                        in
                        ( { model
                            | gameModel = GameModelHM { gm | words = model.words }
                            , status = INIT
                            }
                        , Cmd.none )
                    _ -> ( { model
                            | gameModel = NoGame
                            , status = MENU
                            }, Cmd.none )
            PauseGame ->
                ( { model
                    | status = PAUSED
                }, Cmd.none
                )
            ResumeGame ->
                ( { model
                    | status = IN_GAME
                }, Cmd.none
                )
            CompleteGame ->
                ( { model
                    | status = GAMEOVER
                }, Cmd.none
                )
            GameMsg (GR msg_) ->
                let
                    ( updatedModel, cmd ) = case model.gameModel of
                        GameModelGR gameModel ->
                            let
                                ( gameModel_, grCmd ) = GR.update (GameMsg msg_) gameModel
                            in
                            ( { model | gameModel = GameModelGR gameModel_ }
                            , Cmd.map (\a -> case a of
                                    GameMsg a_ -> GameMsg (GR a_)
                                    CompleteGame -> CompleteGame
                                    _ -> NoOp
                                ) grCmd
                            )
                        _ -> ( model, Cmd.none )
                in ( updatedModel,  cmd )
            GameMsg (HM msg_) ->
                let
                    ( updatedModel, cmd ) = case model.gameModel of
                            GameModelHM gameModel ->
                                let
                                    ( gameModel_, hmCmd ) = HM.update (GameMsg msg_) gameModel
                                in
                                ( { model | gameModel = GameModelHM gameModel_ }
                                , Cmd.map (\a -> case a of
                                        GameMsg a_ -> GameMsg (HM a_)
                                        CompleteGame -> CompleteGame
                                        _ -> NoOp
                                    ) hmCmd
                                )
                            _ -> ( model, Cmd.none )
                in ( updatedModel,  cmd )
            _ -> ( model, Cmd.none )
        )


subscriptions : Model GameModel -> Sub AppMsg
subscriptions model =
    case model.gameModel of
        GameModelGR gameModel_ ->
            let
                model_ = { screensize = model.screensize
                        , status = model.status
                        , gameModel = gameModel_
                        , count = model.count
                        , words = model.words
                        , showingCollections = model.showingCollections
                        , collections = model.collections
                        }
            in
            Sub.map (GR >> GameMsg) (GR.subscriptions model_)
        GameModelHM gameModel_ -> Sub.map (HM >> GameMsg) (HM.subscriptions gameModel_)
        NoGame -> Sub.none


view : Model GameModel -> Document AppMsg
view model =
    let
        gameModelToHM : Model GameModel -> Model (Maybe HM.HMModel)
        gameModelToHM = modelMap (\gameModel_ ->
                            case gameModel_ of
                                GameModelHM a -> Just a
                                _ -> Nothing
                        )

        gameModelToGR : Model GameModel -> Model (Maybe GR.GRModel)
        gameModelToGR = modelMap (\gameModel_ ->
                            case gameModel_ of
                                GameModelGR a -> Just a
                                _ -> Nothing
                        )
    in
    { title = "WordGame - ELM 2021"
    , body =
        [ model.collections
                |> collectionListElement SelectFile (ShowCollection False) model.showingCollections
        ] ++
        ( case model.gameModel of
            NoGame -> chooseGameTypePage SelectGame
            GameModelGR _ ->
                GR.view (gameModelToGR model) |> gameViewMap (gameMsgMapper GR)
            GameModelHM _ ->
                HM.view (gameModelToHM model) |> gameViewMap (gameMsgMapper HM)
        )
    }


gameViewMap : (a -> msg) -> List (Html a) -> List (Html msg)
gameViewMap = List.map << Html.map


gameMsgMapper : (a -> GameMsg) -> Msg a -> AppMsg
gameMsgMapper toGameMsg msg =
    case msg of
        GameMsg a -> GameMsg (toGameMsg a)
        SetScreenSize viewport -> SetScreenSize viewport
        SelectFile string -> SelectFile string
        GotCollections collections -> GotCollections collections
        GotWordList words -> GotWordList words
        ShowCollection bool -> ShowCollection bool
        SelectGame int -> SelectGame int
        StartGame -> StartGame
        PauseGame -> PauseGame
        ResumeGame -> ResumeGame
        CompleteGame -> CompleteGame
        NoOp -> NoOp
