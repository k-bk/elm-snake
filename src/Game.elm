module Game exposing (..)

import Browser
import Browser.Events as Events
import Json.Decode as Decode
import Char exposing (fromCode)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Random exposing (Generator)
import String
import Svg exposing (Svg, rect, svg)
import Svg.Attributes as A
import Time exposing (Posix)


-- MODEL


type alias Model =
    { snake : List Position
    , length : Int
    , direction : Direction
    , input : List Direction
    , points : Int -- not implemented
    , speed : Int -- not implemented
    , state : GameState
    , labirynth : List Position
    , food : Position
    }


type alias Position =
    ( Int, Int )


initModel : Model
initModel =
    { snake = []
    , length = 3
    , direction = Up
    , input = [ Up ]
    , points = 0
    , speed = 8
    , state = Play
    , labirynth = []
    , food = ( 0, 0 )
    }


randomPosition : Generator Position
randomPosition =
    Random.pair (Random.int 0 (boardWidth - 1)) (Random.int 0 (boardHeight - 1))


randomSnake : Cmd Msg
randomSnake =
    Random.generate Restart randomPosition


randomFood : Cmd Msg
randomFood =
    let
        boardRow x =
            List.map (Tuple.pair x) (List.range 0 boardWidth)

        emptyPositions =
            List.map boardRow (List.range 0 boardHeight)
    in
        Random.generate NewFood randomPosition


init : ( Model, Cmd Msg )
init =
    ( initModel, randomSnake )


boardWidth =
    20


boardHeight =
    14



-- UPDATE


type Msg
    = Input Key
    | Tick Posix
    | Restart Position
    | NewFood Position
    | None


type GameState
    = Play
    | GameOver
    | Options MenuOption


type MenuOption
    = ChangeSpeed
    | GameRestart


type Key
    = Arrow Direction
    | Menu
    | Accept
    | Other


type Direction
    = Up
    | Down
    | Left
    | Right


toPosition : Direction -> Position
toPosition direction =
    case direction of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )


opposite : Direction -> Direction
opposite direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.state of
        Play ->
            case msg of
                Input key ->
                    case key of
                        Arrow direction ->
                            ( { model | input = model.input ++ [ direction ] }, Cmd.none )

                        Menu ->
                            ( { model | state = Options ChangeSpeed }, Cmd.none )

                        Accept ->
                            ( model, Cmd.none )

                        Other ->
                            ( model, Cmd.none )

                Tick newTime ->
                    updateSnake model

                Restart position ->
                    ( { initModel | snake = [ position ] }, randomFood )

                NewFood position ->
                    case List.member position (model.snake ++ model.labirynth) of
                        True ->
                            ( model, randomFood )

                        False ->
                            ( { model | food = position }, Cmd.none )

                None ->
                    ( model, Cmd.none )

        Options option ->
            case msg of
                Input key ->
                    case key of
                        Menu ->
                            ( { model | state = Play }, Cmd.none )

                        Arrow Up ->
                            ( { model | state = Options ChangeSpeed }, Cmd.none )

                        Arrow Down ->
                            ( { model | state = Options GameRestart }, Cmd.none )

                        Arrow Right ->
                            if model.state == Options ChangeSpeed then
                                ( { model | speed = model.speed + 1 }, Cmd.none )
                            else
                                ( model, Cmd.none )

                        Arrow Left ->
                            if model.state == Options ChangeSpeed then
                                ( { model | speed = model.speed - 1 }, Cmd.none )
                            else
                                ( model, Cmd.none )

                        Accept ->
                            if model.state == Options GameRestart then
                                init
                            else
                                ( model, Cmd.none )

                        Other ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GameOver ->
            case msg of
                Input Accept ->
                    init

                _ ->
                    ( model, Cmd.none )


updateSnake : Model -> ( Model, Cmd Msg )
updateSnake model =
    let
        shouldDrop direction =
            (direction == model.direction)
                || (direction == (opposite model.direction))

        dropInput input =
            case input of
                direction :: tail ->
                    if shouldDrop direction then
                        dropInput tail
                    else
                        input

                _ ->
                    input

        newInput =
            dropInput model.input

        newDirection =
            case newInput of
                direction :: _ ->
                    direction

                _ ->
                    model.direction

        newSnake =
            moveSnake model.length newDirection model.snake

        hasEaten =
            List.member model.food newSnake
    in
        if hasEaten then
            ( { model
                | snake = newSnake
                , input = newInput
                , direction = newDirection
                , points = model.points + model.speed
                , length = model.length + 1
              }
            , randomFood
            )
        else if isBoardValid newSnake model.labirynth then
            ( { model
                | snake = newSnake
                , input = newInput
                , direction = newDirection
              }
            , Cmd.none
            )
        else
            ( { model | state = GameOver }, Cmd.none )


moveSnake : Int -> Direction -> List Position -> List Position
moveSnake length direction snake =
    let
        pAdd ( x1, y1 ) ( x2, y2 ) =
            ( modBy boardWidth (x1 + x2), modBy boardHeight (y1 + y2) )

        snakeHead =
            case List.head snake of
                Just head ->
                    head

                Nothing ->
                    ( 0, 0 )

        newSnakeHead =
            pAdd snakeHead (toPosition direction)
    in
        List.take length (newSnakeHead :: snake)


isBoardValid : List Position -> List Position -> Bool
isBoardValid snake labirynth =
    case snake of
        head :: tail ->
            let
                snakeValid =
                    not (List.member head tail)

                labirynthValid =
                    not (List.member head labirynth)
            in
                snakeValid && labirynthValid

        _ ->
            False



-- SUBSCRIPTIONS


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    let
        key =
            case string of
                "ArrowLeft" ->
                    Arrow Left

                "ArrowRight" ->
                    Arrow Right

                "ArrowUp" ->
                    Arrow Up

                "ArrowDown" ->
                    Arrow Down

                "Escape" ->
                    Menu

                "Enter" ->
                    Accept

                _ ->
                    Other
    in
        Input key


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (1000 / (toFloat model.speed)) Tick
        , Events.onKeyPress keyDecoder
        ]



-- VIEW


view : Model -> Html msg
view model =
    let
        myStyle =
            [ style "margin" "auto"
            , style "width" "800px"
            ]
    in
        case model.state of
            Play ->
                div myStyle
                    [ svg [ A.viewBox "0 0  640 480" ]
                        [ board model
                        , hud model
                        ]
                    ]

            Options option ->
                div myStyle
                    [ svg [ A.viewBox "0 0  640 480" ]
                        [ board model
                        , hud model
                        , menuOptions model
                        ]
                    ]

            GameOver ->
                div myStyle
                    [ svg [ A.viewBox "0 0  640 480" ]
                        [ board model
                        , hud model
                        , gameOver model
                        ]
                    ]


blockSize =
    32


screenWidth =
    640


screenHeight =
    480


gameOver : Model -> Svg msg
gameOver model =
    let
        gameOverBackground =
            Svg.rect
                [ A.fill "white"
                , A.fillOpacity "0.7"
                , A.width "100%"
                , A.height "94%"
                ]
                []

        gameOverText =
            Svg.text_
                [ A.x "50%"
                , A.y "40%"
                , A.fontWeight "bolder"
                , A.fontSize "xx-large"
                , A.fontFamily "monospace"
                , A.textAnchor "middle"
                , A.fill "red"
                ]
                [ Svg.text "Game over!"
                , Svg.text ("Score: " ++ (String.fromInt model.points))
                ]
    in
        svg [ A.viewBox "0 0 640 480" ]
            [ gameOverBackground
            , gameOverText
            ]


menuOptions : Model -> Svg msg
menuOptions model =
    let
        optionsBackground =
            Svg.rect
                [ A.fill "white"
                , A.fillOpacity "0.7"
                , A.width "100%"
                , A.height "94%"
                ]
                []

        menuOption text yPos active =
            let
                activeColor =
                    if active then
                        "red"
                    else
                        "black"
            in
                Svg.text_
                    [ A.x "50%"
                    , A.y yPos
                    , A.fontWeight "bolder"
                    , A.fontSize "xx-large"
                    , A.fontFamily "monospace"
                    , A.textAnchor "middle"
                    , A.fill activeColor
                    ]
                    [ Svg.text text ]

        isActive option =
            model.state == (Options option)

        speedButton =
            menuOption
                ("Speed: " ++ (String.fromInt model.speed))
                "20%"
                (isActive ChangeSpeed)

        restartButton =
            menuOption
                "Restart"
                "30%"
                (isActive GameRestart)
    in
        svg
            [ A.viewBox "0 0 640 480"
            ]
            (optionsBackground :: speedButton :: restartButton :: [])


hud : Model -> Svg msg
hud model =
    let
        hudBackground =
            Svg.rect
                [ A.width "640"
                , A.height "32"
                , A.fill "red"
                ]
                []

        pointCounter =
            Svg.text_
                [ A.x "10%"
                , A.y "60%"
                , A.fontWeight "bolder"
                , A.fontFamily "monospace"
                , A.fill "white"
                ]
                [ Svg.text ("Score: " ++ (String.fromInt model.points)) ]

        speed =
            Svg.text_
                [ A.x "60%"
                , A.y "60%"
                , A.fontWeight "bolder"
                , A.fontFamily "monospace"
                , A.fill "white"
                ]
                [ Svg.text ("Speed: " ++ (String.fromInt model.speed)) ]
    in
        svg
            [ A.viewBox "0 0 640 32"
            , A.y "47%"
            ]
            [ hudBackground
            , pointCounter
            , speed
            ]


board : Model -> Svg msg
board model =
    let
        block blockColor ( x, y ) =
            rect
                [ A.x (String.fromInt (x * blockSize))
                , A.y (String.fromInt (y * blockSize))
                , A.rx "5"
                , A.ry "5"
                , A.width (String.fromInt blockSize)
                , A.height (String.fromInt blockSize)
                , A.fill blockColor
                ]
                []

        showSnake =
            List.map (block "black") model.snake

        showFood ( x, y ) =
            rect
                [ A.x (String.fromInt (x * blockSize))
                , A.y (String.fromInt (y * blockSize))
                , A.rx "5"
                , A.ry "5"
                , A.width (String.fromInt blockSize)
                , A.height (String.fromInt blockSize)
                , A.fill "red"
                ]
                []

        background =
            rect
                [ A.width (String.fromInt (boardWidth * blockSize))
                , A.height (String.fromInt (boardHeight * blockSize))
                , A.fill "pink"
                ]
                []
    in
        svg
            [ A.viewBox "0 0 640 480"
            ]
            (background :: showFood model.food :: showSnake)



-- MAIN


main =
    Browser.element
        { init = \() -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
