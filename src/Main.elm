module Main exposing (main, view)

import Browser
import Browser.Events exposing (onKeyDown)
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List.Extra as List
import Random
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Tetrimino
    = I
    | O
    | S
    | Z
    | J
    | L
    | T


intToTetrimino : Int -> Tetrimino
intToTetrimino i =
    case i of
        0 ->
            I

        1 ->
            O

        2 ->
            S

        3 ->
            Z

        4 ->
            J

        5 ->
            L

        _ ->
            T


type Tile
    = None
    | Wall
    | Block
    | Tetrimino Tetrimino


isTetrimino : Tile -> Bool
isTetrimino tile =
    case tile of
        Tetrimino _ ->
            True

        _ ->
            False


tileToInt : Tile -> Int
tileToInt tile =
    case tile of
        None ->
            0

        Wall ->
            1

        Block ->
            2

        Tetrimino _ ->
            3


type alias Model =
    { board : List Tile
    }


init : ( Model, Cmd Msg )
init =
    let
        board =
            List.initialize 200 (\n -> None) ++ List.initialize 10 (\n -> Wall)
    in
    ( { board = putTetrimino 4 0 I board }, Cmd.none )


putTetrimino : Int -> Int -> Tetrimino -> List Tile -> List Tile
putTetrimino x y tetrimino board =
    case tetrimino of
        I ->
            List.updateAt ((y + 1) * 10 + x) (\_ -> Tetrimino I) board
                |> List.updateAt ((y + 1) * 10 + x - 1) (\_ -> Tetrimino I)
                |> List.updateAt ((y + 1) * 10 + x + 1) (\_ -> Tetrimino I)
                |> List.updateAt ((y + 1) * 10 + x + 2) (\_ -> Tetrimino I)

        O ->
            List.updateAt (y * 10 + x) (\_ -> Tetrimino O) board
                |> List.updateAt (y * 10 + x + 1) (\_ -> Tetrimino O)
                |> List.updateAt ((y + 1) * 10 + x) (\_ -> Tetrimino O)
                |> List.updateAt ((y + 1) * 10 + x + 1) (\_ -> Tetrimino O)

        T ->
            List.updateAt (y * 10 + x) (\_ -> Tetrimino T) board
                |> List.updateAt ((y + 1) * 10 + x) (\_ -> Tetrimino T)
                |> List.updateAt ((y + 1) * 10 + x - 1) (\_ -> Tetrimino T)
                |> List.updateAt ((y + 1) * 10 + x + 1) (\_ -> Tetrimino T)

        J ->
            List.updateAt (y * 10 + x - 1) (\_ -> Tetrimino J) board
                |> List.updateAt ((y + 1) * 10 + x - 1) (\_ -> Tetrimino J)
                |> List.updateAt ((y + 1) * 10 + x) (\_ -> Tetrimino J)
                |> List.updateAt ((y + 1) * 10 + x + 1) (\_ -> Tetrimino J)

        L ->
            List.updateAt (y * 10 + x + 1) (\_ -> Tetrimino L) board
                |> List.updateAt ((y + 1) * 10 + x - 1) (\_ -> Tetrimino L)
                |> List.updateAt ((y + 1) * 10 + x) (\_ -> Tetrimino L)
                |> List.updateAt ((y + 1) * 10 + x + 1) (\_ -> Tetrimino L)

        S ->
            List.updateAt (y * 10 + x) (\_ -> Tetrimino S) board
                |> List.updateAt (y * 10 + x + 1) (\_ -> Tetrimino S)
                |> List.updateAt ((y + 1) * 10 + x) (\_ -> Tetrimino S)
                |> List.updateAt ((y + 1) * 10 + x - 1) (\_ -> Tetrimino S)

        Z ->
            List.updateAt (y * 10 + x - 1) (\_ -> Tetrimino Z) board
                |> List.updateAt (y * 10 + x) (\_ -> Tetrimino Z)
                |> List.updateAt ((y + 1) * 10 + x) (\_ -> Tetrimino Z)
                |> List.updateAt ((y + 1) * 10 + x + 1) (\_ -> Tetrimino Z)


generateBlock : Cmd Msg
generateBlock =
    Random.map intToTetrimino (Random.int 0 6) |> Random.generate NewTetrimino


canMove : List Tile -> Direction -> Bool
canMove board direction =
    let
        indices =
            List.findIndices isTetrimino board

        aux i b =
            case List.getAt i b of
                Just Wall ->
                    False

                Just Block ->
                    False

                _ ->
                    True
    in
    case direction of
        Left ->
            List.all (\i -> modBy 10 i /= 0 && aux (i - 1) board) indices

        Right ->
            List.all (\i -> modBy 10 i /= 9 && aux (i + 1) board) indices

        Down ->
            List.all (\i -> aux (i + 10) board) indices

        _ ->
            False


moveTetrimino : List Tile -> Direction -> List Tile
moveTetrimino board direction =
    let
        indices =
            List.findIndices isTetrimino board
    in
    case direction of
        Left ->
            List.indexedMap
                (\i ->
                    \x ->
                        case List.find ((==) i) indices of
                            Just _ ->
                                case List.find ((==) (i + 1)) indices of
                                    Just _ ->
                                        x

                                    Nothing ->
                                        None

                            Nothing ->
                                case List.find ((==) (i + 1)) indices of
                                    Just index ->
                                        List.getAt index board |> Maybe.withDefault x

                                    Nothing ->
                                        x
                )
                board

        Right ->
            List.indexedMap
                (\i ->
                    \x ->
                        case List.find ((==) i) indices of
                            Just _ ->
                                case List.find ((==) (i - 1)) indices of
                                    Just _ ->
                                        x

                                    Nothing ->
                                        None

                            Nothing ->
                                case List.find ((==) (i - 1)) indices of
                                    Just index ->
                                        List.getAt index board |> Maybe.withDefault x

                                    Nothing ->
                                        x
                )
                board

        Down ->
            List.indexedMap
                (\i ->
                    \x ->
                        case List.find ((==) i) indices of
                            Just _ ->
                                case List.find ((==) (i - 10)) indices of
                                    Just _ ->
                                        x

                                    Nothing ->
                                        None

                            Nothing ->
                                case List.find ((==) (i - 10)) indices of
                                    Just index ->
                                        List.getAt index board |> Maybe.withDefault x

                                    Nothing ->
                                        x
                )
                board

        _ ->
            board


toBlock : List Tile -> List Tile
toBlock board =
    List.map
        (\x ->
            if isTetrimino x then
                Block

            else
                x
        )
        board


vanishBlock : List Tile -> List Tile
vanishBlock board =
    List.groupsOf 10 board
        |> List.map
            (\x ->
                let
                    sort =
                        List.unique (List.map tileToInt x)
                in
                if List.length sort == 1 && List.head sort == Just (tileToInt Block) then
                    []

                else
                    x
            )
        |> List.concat
        |> (\b -> List.initialize (210 - List.length b) (\_ -> None) ++ b)



-- UPDATE


type Msg
    = Change Direction
    | Tick Time.Posix
    | NewTetrimino Tetrimino


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if canMove model.board Down then
                ( { board = moveTetrimino model.board Down }, Cmd.none )

            else
                ( { board = toBlock model.board |> vanishBlock }, generateBlock )

        Change direction ->
            if canMove model.board direction then
                ( { model | board = moveTetrimino model.board direction }, Cmd.none )

            else
                ( model, Cmd.none )

        NewTetrimino x ->
            ( { board = putTetrimino 4 0 x model.board }, Cmd.none )



-- SUBSCRIPTIONS


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        _ ->
            Other


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 100 Tick
        , onKeyDown (Decode.map Change keyDecoder)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    layout [] (wrappedRow [ width (px 300) ] (List.map viewTile model.board))


viewTile : Tile -> Element Msg
viewTile tile =
    el
        [ width (px 30)
        , height (px 30)
        ]
    <|
        case tile of
            None ->
                el [ width fill, height fill, Background.color (rgb255 0 0 0), Border.rounded 2 ] none

            Wall ->
                el [ width fill, height fill, Background.color (rgb255 128 128 128), Border.rounded 2 ] none

            Block ->
                el [ width fill, height fill, Background.color (rgb255 117 83 43), Border.rounded 2 ] none

            Tetrimino sort ->
                case sort of
                    I ->
                        -- 水色
                        el [ width fill, height fill, Background.color (rgb255 157 204 224), Border.rounded 2 ] none

                    O ->
                        -- 黄色
                        el [ width fill, height fill, Background.color (rgb255 255 255 102), Border.rounded 2 ] none

                    S ->
                        -- 緑
                        el [ width fill, height fill, Background.color (rgb255 0 153 102), Border.rounded 2 ] none

                    Z ->
                        -- 赤
                        el [ width fill, height fill, Background.color (rgb255 235 50 40), Border.rounded 2 ] none

                    J ->
                        -- 青
                        el [ width fill, height fill, Background.color (rgb255 51 102 153), Border.rounded 2 ] none

                    L ->
                        -- オレンジ
                        el [ width fill, height fill, Background.color (rgb255 239 129 15), Border.rounded 2 ] none

                    T ->
                        -- 紫
                        el [ width fill, height fill, Background.color (rgb255 153 51 153), Border.rounded 2 ] none
