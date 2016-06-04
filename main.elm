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
  { ball = Ball 0 0 24 12
  , bricks = []
  , bid = 0
  }


init : (Model, Cmd Msg)
init =
  (defaultModel, Cmd.none)

-- Update

type Msg =
  Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick delta ->
      let
        { ball } = model
        newBall = updateBall delta ball
      in
        ({ model | ball = newBall }, Cmd.none)


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
view { ball } =
  svg
    [ width (toString gameWidth), height (toString gameHeight), viewBox "0 0 100 100" ]
    [ circle [ cx (toString ball.x), cy (toString ball.y), r (toString ballSize) ] [] ]




