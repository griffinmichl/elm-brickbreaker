import Html exposing (Html, text)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Time exposing (..)

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
  }

type alias Model = 
  { ball : Ball
  , bricks : List Brick
  , bid : Int
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
  in
    List.map createBrick [0..num-1]


defaultModel : Model
defaultModel =
  { ball = Ball (gameWidth / 2) (gameHeight - 11) 100 -75 10
  , bricks = generateBricks 20 5 
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
        newBall = updateBall delta ball bricks
        newBricks = removeCollisions ball bricks
      in
        ({ model | ball = newBall, bricks = newBricks }, Cmd.none)

removeCollisions : Ball -> List Brick -> List Brick
removeCollisions ball bricks = List.filter (not << isBrickCollision ball) bricks

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


updateBall : Time -> Ball -> List Brick -> Ball
updateBall dt ball bricks =
  physicsUpdate dt <| updateDirection ball bricks

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


updateDirection : Ball -> List Brick -> Ball
updateDirection ball bricks =
  { ball |
    vx = getDirection ball.vx (near ball.r 0 ball.x || near ball.r gameWidth ball.x || anyVerticalCollision ball bricks)
  , vy = getDirection ball.vy (near ball.r 0 ball.y || near ball.r gameHeight ball.y || anyHorizontalCollision ball bricks)
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
  AnimationFrame.diffs (Tick << inSeconds)

-- View

view : Model -> Html Msg
view { ball, bricks } =
  svg
    [ width (toString gameWidth), height (toString gameHeight) ]
    ([ circle
      [ cx (toString ball.x), cy (toString ball.y), r (toString ball.r) ]
      []
    ] ++ List.map renderBrick bricks)

renderBrick : Brick -> Html Msg
renderBrick brick =
  rect
    [ x (toString brick.x)
    , y (toString brick.y)
    , height (toString brick.height)
    , width (toString brick.width)
    , fill "green"
    , stroke "black"
    ]
    []

