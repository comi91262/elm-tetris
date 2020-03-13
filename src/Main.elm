module Main exposing (Direction(..), Model, Msg(..), Pivot, Shape(..), Tile(..), canMove, createTetrimino, deleteLine, deleteTetorimino, generateBlock, getPivot, getShape, init, intToShape, isTetrimino, isWallorBlack, keyDecoder, main, moveTetrimino, putTetorimino, stage, subscriptions, toBlock, toDirection, update, view, viewTile)

import Browser
import Browser.Events exposing (onKeyDown)
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


type Shape
    = I
    | O
    | S
    | Z
    | J
    | L
    | T


intToShape : Int -> Shape
intToShape i =
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


type alias Pivot =
    ( Int, Int )


type Tile
    = None
    | Wall
    | Block
    | Tetrimino Shape Pivot


isTetrimino : Tile -> Bool
isTetrimino tile =
    case tile of
        Tetrimino _ _ ->
            True

        _ ->
            False


stage : { width : Int, height : Int, invisibleArea : Int }
stage =
    { width = 12, height = 21, invisibleArea = 12 * 2 }


type alias Model =
    { board : List Tile
    }


init : ( Model, Cmd Msg )
init =
    let
        stageSize =
            stage.width * stage.height

        board =
            List.initialize (stage.invisibleArea + stageSize) (\n -> None)
                |> List.indexedMap
                    (\i tail ->
                        let
                            x =
                                modBy stage.width i
                        in
                        if x == 0 || x == stage.width - 1 || i > stage.invisibleArea + stageSize - stage.width then
                            Wall

                        else
                            tail
                    )
    in
    ( { board = createTetrimino I board }, Cmd.none )


createTetrimino : Shape -> List Tile -> List Tile
createTetrimino shape board =
    let
        toIndex ( x1, y1 ) =
            y1 * stage.width + x1
    in
    case shape of
        I ->
            putTetorimino (List.map toIndex [ ( 3, 1 ), ( 4, 1 ), ( 5, 1 ), ( 6, 1 ) ]) shape ( 4, 0 ) board

        O ->
            putTetorimino (List.map toIndex [ ( 4, 0 ), ( 5, 0 ), ( 4, 1 ), ( 5, 1 ) ]) shape ( 4, 0 ) board

        T ->
            putTetorimino (List.map toIndex [ ( 3, 1 ), ( 4, 0 ), ( 4, 1 ), ( 5, 1 ) ]) shape ( 4, 1 ) board

        J ->
            putTetorimino (List.map toIndex [ ( 3, 0 ), ( 3, 1 ), ( 4, 1 ), ( 5, 1 ) ]) shape ( 4, 1 ) board

        L ->
            putTetorimino (List.map toIndex [ ( 3, 1 ), ( 4, 1 ), ( 5, 0 ), ( 5, 1 ) ]) shape ( 4, 1 ) board

        S ->
            putTetorimino (List.map toIndex [ ( 3, 1 ), ( 4, 0 ), ( 4, 1 ), ( 5, 0 ) ]) shape ( 4, 1 ) board

        Z ->
            putTetorimino (List.map toIndex [ ( 3, 0 ), ( 4, 0 ), ( 4, 1 ), ( 5, 1 ) ]) shape ( 4, 1 ) board


generateBlock : Cmd Msg
generateBlock =
    Random.map intToShape (Random.int 0 6) |> Random.generate NewTetrimino


getPivot : List Int -> List Tile -> Pivot
getPivot indices board =
    List.head indices
        |> Maybe.andThen (\index -> List.getAt index board)
        |> Maybe.andThen
            (\tail ->
                case tail of
                    Tetrimino _ pivot ->
                        Just pivot

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault ( 0, 0 )


getShape : List Int -> List Tile -> Shape
getShape indices board =
    List.head indices
        |> Maybe.andThen (\index -> List.getAt index board)
        |> Maybe.andThen
            (\x ->
                case x of
                    Tetrimino shape _ ->
                        Just shape

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault I


isWallorBlack : Int -> List Tile -> Bool
isWallorBlack index board =
    case List.getAt index board of
        Just Wall ->
            True

        Just Block ->
            True

        _ ->
            False


canMove : List Tile -> Direction -> Bool
canMove board direction =
    let
        indices =
            List.findIndices isTetrimino board
    in
    case direction of
        Left ->
            List.all (\i -> not (isWallorBlack (i - 1) board)) indices

        Right ->
            List.all (\i -> not (isWallorBlack (i + 1) board)) indices

        Down ->
            List.all (\i -> not (isWallorBlack (i + stage.width) board)) indices

        Up ->
            let
                shape =
                    getShape indices board

                ( pivotX, pivotY ) =
                    getPivot indices board

                updatedIndices =
                    List.map (\i -> ( modBy stage.width i, i // stage.width )) indices
                        |> List.map
                            (\( x, y ) ->
                                case shape of
                                    I ->
                                        ( pivotY - y + pivotX + 1, x - pivotX + pivotY )

                                    O ->
                                        ( x, y )

                                    _ ->
                                        ( pivotY - y + pivotX, x - pivotX + pivotY )
                            )
                        |> List.map (\( x, y ) -> y * stage.width + x)
            in
            List.all (\i -> not (isWallorBlack i board)) updatedIndices

        _ ->
            False


moveTetrimino : List Tile -> Direction -> List Tile
moveTetrimino board direction =
    let
        indices =
            List.findIndices isTetrimino board

        shape =
            getShape indices board

        ( pivotX, pivotY ) =
            getPivot indices board
    in
    case direction of
        Left ->
            let
                updatedIndices =
                    List.map (\i -> i - 1) indices
            in
            deleteTetorimino board
                |> putTetorimino updatedIndices shape ( pivotX - 1, pivotY )

        Right ->
            let
                updatedIndices =
                    List.map (\i -> i + 1) indices
            in
            deleteTetorimino board
                |> putTetorimino updatedIndices shape ( pivotX + 1, pivotY )

        Down ->
            let
                updatedIndices =
                    List.map (\i -> i + stage.width) indices
            in
            deleteTetorimino board
                |> putTetorimino updatedIndices shape ( pivotX, pivotY + 1 )

        Up ->
            let
                updatedIndices =
                    List.map (\i -> ( modBy stage.width i, i // stage.width )) indices
                        |> List.map
                            (\( x, y ) ->
                                case shape of
                                    I ->
                                        ( pivotY - y + pivotX + 1, x - pivotX + pivotY )

                                    O ->
                                        ( x, y )

                                    _ ->
                                        ( pivotY - y + pivotX, x - pivotX + pivotY )
                            )
                        |> List.map (\( x, y ) -> y * stage.width + x)
            in
            deleteTetorimino board
                |> putTetorimino updatedIndices shape ( pivotX, pivotY )

        _ ->
            board


putTetorimino : List Int -> Shape -> Pivot -> List Tile -> List Tile
putTetorimino indices shape pivot board =
    List.indexedMap
        (\index element ->
            case List.find ((==) index) indices of
                Just _ ->
                    Tetrimino shape pivot

                Nothing ->
                    element
        )
        board


deleteTetorimino : List Tile -> List Tile
deleteTetorimino board =
    List.map
        (\x ->
            if isTetrimino x then
                None

            else
                x
        )
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


deleteLine : List Tile -> List Tile
deleteLine board =
    List.groupsOf stage.width board
        |> List.map
            (\line ->
                if List.count (\x -> x == Block) line == stage.width - 2 then
                    []

                else
                    line
            )
        |> List.concat
        |> (\b ->
                let
                    newLines =
                        List.initialize (stage.height * stage.width + stage.invisibleArea - List.length b) (\_ -> None)
                            |> List.indexedMap
                                (\i tail ->
                                    let
                                        x =
                                            modBy stage.width i
                                    in
                                    if x == 0 || x == stage.width - 1 then
                                        Wall

                                    else
                                        tail
                                )
                in
                newLines ++ b
           )



-- UPDATE


type Msg
    = Change Direction
    | Tick Time.Posix
    | NewTetrimino Shape


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
                ( { board = toBlock model.board |> deleteLine }, generateBlock )

        Change direction ->
            if canMove model.board direction then
                ( { model | board = moveTetrimino model.board direction }, Cmd.none )

            else
                ( model, Cmd.none )

        NewTetrimino shape ->
            ( { board = createTetrimino shape model.board }, Cmd.none )



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
        [ Time.every 1000 Tick
        , onKeyDown (Decode.map Change keyDecoder)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    layout [] (wrappedRow [ width (px (30 * stage.width)) ] (List.map viewTile (List.drop stage.invisibleArea model.board)))


viewTile : Tile -> Element Msg
viewTile tile =
    el
        [ width (px 30)
        , height (px 30)
        ]
    <|
        let
            makeTile color =
                el [ width fill, height fill, color, Border.rounded 2 ] none
        in
        case tile of
            None ->
                makeTile (Background.color (rgb255 0 0 0))

            Wall ->
                makeTile (Background.color (rgb255 128 128 128))

            Block ->
                makeTile (Background.color (rgb255 117 83 43))

            Tetrimino shape _ ->
                case shape of
                    I ->
                        -- 水色
                        makeTile (Background.color (rgb255 157 204 224))

                    O ->
                        -- 黄色
                        makeTile (Background.color (rgb255 255 255 102))

                    S ->
                        -- 緑
                        makeTile (Background.color (rgb255 0 153 102))

                    Z ->
                        -- 赤
                        makeTile (Background.color (rgb255 235 50 40))

                    J ->
                        -- 青
                        makeTile (Background.color (rgb255 51 102 153))

                    L ->
                        -- オレンジ
                        makeTile (Background.color (rgb255 239 129 15))

                    T ->
                        -- 紫
                        makeTile (Background.color (rgb255 153 51 153))
