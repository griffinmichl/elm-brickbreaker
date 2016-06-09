import Html exposing (Html, div, span, text)
import Html.App as App
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Time exposing (..)
import Keyboard

import Debug

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model

(gameWidth, gameHeight) = (500, 500)

type alias Ball =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , r : Float
  }

type alias Brick =
  { x : Float
  , y : Float
  , height : Float
  , width : Float
  , id : Int
  , health : Int
  }

type alias Paddle =
  { x : Float
  , y : Float
  , height : Float
  , width : Float
  , vx : Float
  , dir : Int
  }

type alias Game =
  { lives : Int
  }

type alias Model = 
  { ball : Ball
  , bricks : List Brick
  , bid : Int
  , paddle : Paddle
  , game : Game
  }

generateBricks : Int -> Int -> List Brick
generateBricks num numPerRow = 
  let
    width = (gameWidth // numPerRow)
    height = width // 3
    createBrick i =
      Brick
        (toFloat ((i % numPerRow) * width))
        (toFloat ((i // numPerRow) * height))
        (toFloat height)
        (toFloat width)
        i
        3
  in
    List.map createBrick [0..num-1]

defaultBall : Ball
defaultBall = Ball (gameWidth / 2) (gameHeight - 11) 100 -75 10


defaultModel : Model
defaultModel =
  { ball = defaultBall
  , bricks = generateBricks 20 5 
  , bid = 0
  , paddle = Paddle (gameWidth / 2) (gameHeight - 5) 5 100 5 0
  , game = Game 3
  }


init : (Model, Cmd Msg)
init =
  (defaultModel, Cmd.none)

-- Update

type Msg
  = Tick Time
  | Direction Int
  | Static

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Static ->
      (model, Cmd.none)

    Direction dir ->
      let
        { paddle } = model
      in
        ({ model | paddle = { paddle | dir = dir } }, Cmd.none)

    Tick delta ->
      let
        { ball, bricks, paddle, game } = model
        newBall = updateBall delta ball paddle bricks
        newBricks = updateBricks ball bricks
        newPaddle = updatePaddle paddle
        newGame = updateGame game newPaddle newBall
      in
        if newGame.lives == 0 then
           (defaultModel, Cmd.none)
        else
          ({ model |
             ball = if newGame.lives /= game.lives then defaultBall else newBall,
             bricks = newBricks,
             paddle = newPaddle,
             game = newGame
           }, Cmd.none)

updateGame : Game -> Paddle -> Ball -> Game
updateGame game paddle ball =
  if isOutOfBounds ball paddle then
    { game | lives = game.lives - 1 }
  else
    game

updatePaddle : Paddle -> Paddle
updatePaddle paddle =
  { paddle | x = paddle.x + (toFloat paddle.dir) * paddle.vx }

updateHealth : Ball -> List Brick -> List Brick
updateHealth ball bricks =
  let
    update b brick =
      if isBrickCollision b brick then
        { brick | health = brick.health - 1 }
      else
        brick
  in
    List.map (update ball) bricks

removeDestroyed : Ball -> List Brick -> List Brick
removeDestroyed ball bricks = List.filter (\brick -> brick.health > 0) bricks

updateBricks : Ball -> List Brick -> List Brick
updateBricks ball bricks = removeDestroyed ball <| updateHealth ball bricks

isOutOfBounds : Ball -> Paddle -> Bool
isOutOfBounds ball paddle = 
     near ball.r gameHeight ball.y
  && (not <| near (paddle.width / 2) ball.x (paddle.x + paddle.width / 2 ))


isHorizontalCollision : Ball -> Brick -> Bool
isHorizontalCollision ball brick =
  let
    distanceX = abs (ball.x - (brick.x + brick.width / 2))
  in
    distanceX <= (brick.width / 2 + ball.r) && distanceX < (brick.width / 2)

isVerticalCollision : Ball -> Brick -> Bool
isVerticalCollision ball brick =
  let
    distanceY = abs (ball.y - (brick.y + brick.height / 2))
  in
    distanceY <= (brick.height / 2 + ball.r) && distanceY <= (brick.height / 2)
    
isDiagCollision : Ball -> Brick -> Bool
isDiagCollision ball brick =
  let
    distanceX = abs (ball.x - (brick.x + brick.width / 2))
    distanceY = abs (ball.y - (brick.y + brick.height / 2))
  in
    ((distanceX - brick.width / 2) ^ 2 + (distanceY - brick.height / 2) ^ 2) <= ball.r ^ 2

isBrickCollision : Ball -> Brick -> Bool
isBrickCollision ball brick =
  let
    distanceX = abs (ball.x - (brick.x + brick.width / 2))
    distanceY = abs (ball.y - (brick.y + brick.height / 2))
  in
    if distanceX > (brick.width / 2 + ball.r) then
      False
    else if distanceY > (brick.height / 2 + ball.r) then
      False
    else if distanceX <= (brick.width / 2) then
      True
    else if distanceY <= (brick.height / 2) then
      True
    else
      (
        (distanceX - brick.width / 2) ^ 2 +
        (distanceY - brick.height / 2) ^ 2
      ) <= ball.r ^ 2

isPaddleCollision : Ball -> Paddle -> Bool
isPaddleCollision ball paddle = 
     near ball.r gameHeight ball.y
  && near (paddle.width / 2) ball.x (paddle.x + paddle.width / 2 )

updateBall : Time -> Ball -> Paddle -> List Brick -> Ball
updateBall dt ball paddle bricks =
  physicsUpdate dt <| updateSpeed paddle <| updateDirection ball paddle bricks

updateSpeed : Paddle -> Ball -> Ball
updateSpeed paddle ball = 
  if isPaddleCollision ball paddle then
    { ball | vx = ball.vx + ball.vx / 10, vy = ball.vy + ball.vy / 10 }
  else
    ball

anyVerticalCollision : Ball -> List Brick -> Bool
anyVerticalCollision ball bricks =
  List.any
    (\brick -> isVerticalCollision ball brick || isDiagCollision ball brick)
    <| List.filter (isBrickCollision ball) bricks

anyHorizontalCollision : Ball -> List Brick -> Bool
anyHorizontalCollision ball bricks =
  List.any
    (\brick -> isHorizontalCollision ball brick || isDiagCollision ball brick)
    <| List.filter (isBrickCollision ball) bricks


updateDirection : Ball -> Paddle -> List Brick -> Ball
updateDirection ball paddle bricks =
  { ball |
    vx = getDirection ball.vx (near ball.r 0 ball.x || near ball.r gameWidth ball.x || anyVerticalCollision ball bricks)
  , vy = getDirection ball.vy (isPaddleCollision ball paddle || near ball.r 0 ball.y || anyHorizontalCollision ball bricks)
  }


near : Float -> Float -> Float -> Bool
near dist from to =
  to >= from - dist && to <= from + dist


physicsUpdate : Time -> Ball -> Ball
physicsUpdate dt obj =
  { obj |
    x = obj.x + obj.vx * dt,
    y = obj.y + obj.vy * dt
  }

getDirection : Float -> Bool -> Float
getDirection v collision =
  if collision then
    -v
  else
    v

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs (Tick << inSeconds)
    , Keyboard.downs <| processKey True
    , Keyboard.ups <| processKey False
    ]

processKey : Bool -> Keyboard.KeyCode -> Msg
processKey bool keycode =
  case (bool, keycode) of
    (False, _) ->
      Direction 0

    (_, 37) ->
      Direction 1

    (_, 39) ->
      Direction -1

    _ -> Static


-- View

view : Model -> Html Msg
view { ball, bricks, paddle, game } =
  div
    [ Html.Attributes.style [("padding", "50px")] ]
    [ svg
        [ Svg.Attributes.style "outline: thin solid black"
        , width (toString gameWidth), height (toString gameHeight)
        ]
        (
          [ circle
              [ cx (toString ball.x), cy (toString ball.y), r (toString ball.r) ]
              []
          , renderPaddle paddle
          ] ++ List.map renderBrick bricks
        ),
      span
        [ Html.Attributes.style [("vertical-align", "top")] ]
        [ Html.text ("Lives: " ++ toString game.lives) ]
    ]

renderPaddle : Paddle -> Html Msg
renderPaddle paddle =
  rect
    [ x (toString paddle.x)
    , y (toString paddle.y)
    , height (toString paddle.height)
    , width (toString paddle.width)
    ]
    []

getBrickColor : Brick -> String
getBrickColor { health } =
  case health of
    1 -> "LightBlue"
    2 -> "Blue"
    3 -> "DarkBlue"
    _ -> "Black"

renderBrick : Brick -> Html Msg
renderBrick brick =
  rect
    [ x (toString brick.x)
    , y (toString brick.y)
    , height (toString brick.height)
    , width (toString brick.width)
    , fill <| getBrickColor brick
    , stroke "black"
    ]
    []

