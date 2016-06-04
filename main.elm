import Html exposing (Html, text)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Time exposing (..)

import Debug exposing (log)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model

(gameWidth, gameHeight) = (500, 500)
(ballSize) = (5)

type alias Ball =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  }

type alias Brick =
  { x : Float
  , y : Float
  , height : Float
  , width : Float
  , id : Int
  }

type alias Model = 
  { ball : Ball
  , bricks : List Brick
  , bid : Int
  }

defaultModel : Model
defaultModel =
  { ball = Ball 0 0 12 12
  , bricks = [ Brick 50 50 10 10 0 ]
  , bid = 0
  }


init : (Model, Cmd Msg)
init =
  (defaultModel, Cmd.none)

-- Update

type Msg
  = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick delta ->
      let
        { ball, bricks } = model
        newBall = updateBall delta ball
        newBricks = removeCollisions ball bricks
      in
        ({ model | ball = newBall, bricks = newBricks }, Cmd.none)

removeCollisions : Ball -> List Brick -> List Brick
removeCollisions ball bricks = List.filter (not << isCollision ball) bricks

isCollision : Ball -> Brick -> Bool
isCollision ball brick =
  let
    distanceX = abs (ball.x - (brick.x + brick.width / 2))
    distanceY = abs (ball.y - (brick.y + brick.height / 2))
  in
    if distanceX > (brick.width / 2 + ballSize) then
      False
    else if distanceY > (brick.height / 2 + ballSize) then
      False
    else if distanceX <= (brick.width / 2) then
      True
    else if distanceY <= (brick.height / 2) then
      True
    else
      (
        (distanceX - brick.width / 2) ^ 2 +
        (distanceY - brick.height / 2) ^ 2
      ) <= ballSize ^ 2

updateBall : Time -> Ball -> Ball
updateBall dt ball =
  physicsUpdate dt
    { ball |
      vx = getDirection ball.vx (near ballSize 0 ball.x) (near ballSize 100 ball.x),
      vy = getDirection ball.vy (near ballSize 0 ball.y) (near ballSize 100 ball.y)
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


getDirection : Float -> Bool -> Bool -> Float
getDirection v lowerCollision upperCollision =
  if lowerCollision then
    abs v
  else if upperCollision then
    -(abs v)
  else
    v

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs (Tick << inSeconds)

-- View

view : Model -> Html Msg
view { ball, bricks } =
  svg
    [ width (toString gameWidth), height (toString gameHeight), viewBox "0 0 100 100" ]
    ([ circle
      [ cx (toString ball.x), cy (toString ball.y), r (toString ballSize) ]
      []
    ] ++ List.map renderBrick bricks)

renderBrick : Brick -> Html Msg
renderBrick brick =
  rect
    [ x (toString brick.x)
    , y (toString brick.y)
    , height (toString brick.height)
    , width (toString brick.width)
    ]
    []

