-- See this document for more information on making Pong:
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Debug

-- MODEL

(gameWidth,gameHeight) = (480,600)
(halfWidth,halfHeight) = (240,300)
(xLeftProximity,xRightProximity) = (-40, 40)
(yLowerProximity, yUpperProximity) = (-10, 10)
(maxLevel) = (3)

type State = Play | Pause | Won | Lost | WonLevel1

type alias Ball =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , slowmo: Bool
  , bigball : Bool
  , speedup: Bool
  }

type alias Player =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , score : Int
  }

type alias Brick =
  { x : Float
  , y : Float
  , active : Int
  , slowmo: Bool
  , bigball: Bool
  , speedup: Bool
  }

type alias Game =
  { state : State
  , ball : Ball
  , player : Player
  , bricks: List(Brick)
  , level: Int
  }

player : Float -> Player
player x =
  Player x -250 0 0 0

defaultGame : Game
defaultGame =
  { state = Pause
  , ball = Ball 0 0 200 200 False False False
  , player = player 0
  , bricks = [
      Brick -200 100 1 False True False,
      Brick -100 100 1 False False False,
      Brick 0 100 1 False False False, 
      Brick 100 100 1 False False False,
      Brick 200 100 1 False False False,
      Brick -200 250 1 False False False, 
      Brick -100 250 1 False False False, 
      Brick 0 250 1 True False False, 
      Brick 100 250 1 False False False,
      Brick 200 250 1 False False True
  ]
  , level = 1
  }

type alias Input =
  { space : Bool
  , dir1 : Int
  , delta : Time
  }


-- UPDATE

update : Input -> Game -> Game
update {space,dir1, delta} ({state,ball,player,bricks,level} as game) =
  let
    score1 = 0
    newStateMoment =
      if  | space && state == Play ->
              Pause

          | space && state == Pause ->
              Play

          | 0 == (countBricks bricks) && level < maxLevel ->
              WonLevel1

          | 0 == (countBricks bricks) && level == maxLevel ->
              Won

          | (ball.y < -280) ->
              Lost

          | otherwise ->
              state
    newLevel = if (newState == WonLevel1) 
                 then level + 1
               else 
                 level

    newBall =
      if state == Pause || state == Won || state == Lost then
        ball
      else
        updateBall delta ball player bricks

    newBricks = if (newStateMoment == WonLevel1) 
                  then [Brick -100 100 1 True False False,
              Brick -100 125 1 False False False,
              Brick -100 75 1 False False False,
              Brick -200 100 1 False False False,
              Brick 0 100 1 False False True, 
              Brick 0 125 1 False False False, 
              Brick 0 150 1 False False False, 
              Brick 0 75 1 False False False, 
              Brick 0 50 1 False False False, 
              Brick 100 100 1 False True False,
              Brick 100 125 1 False False False,
              Brick 100 75 1 False False False,
              Brick 200 100 1 False False False]
                else 
                  updateBricks delta bricks ball

    newState = if (newStateMoment == WonLevel1)
                    then Play
               else newStateMoment
  in
    { game |
        state <- newState,
        ball <- newBall,
        player <- updatePlayer delta dir1 score1 player,
        bricks <- newBricks,
        level <- newLevel
    }

updateBall : Time -> Ball -> Player -> List(Brick)-> Ball
updateBall t ({x,y,vx,vy, slowmo, bigball} as ball) p1 bricks =
  if not (ball.x |> near 0 halfWidth) then
    { ball | x <- 0, y <- 0 }
  else
    physicsUpdate t
      { ball |
          bigball <- (brickCollision bricks ball isCollidingBigBallBrickFunction emptyCollidingBigballBrickFunction),
          vx <- stepVx vx vy (x < 7-halfWidth)(x > halfWidth-7),
          vy <- stepVy vx vy (y < 7-halfHeight) (y > halfHeight-7)
                   ( x >= p1.x + xLeftProximity 
                  && x <= p1.x + xRightProximity 
                  && y >= p1.y - 10 
                  && y <= p1.y + 10)
                (brickCollision bricks ball isCollidingBrickFunction emptyCollidingBrickFunction)
                (brickCollision bricks ball brickSpecialMultiplierFunction emptyBrickSpecialMultiplierFunction) 
                ball
      }

brickCollision bricks ball function empty = case bricks of  
                                              [] -> empty [] ball
                                              brick::bricks -> if (inRange ball brick)
                                                                  then function brick ball
                                                               else 
                                                               brickCollision bricks ball function empty

inRange obj1 obj2 = (obj1.x >= obj2.x + xLeftProximity 
                   && obj1.x <= obj2.x + xRightProximity 
                   && obj1.y >= obj2.y + yLowerProximity 
                   && obj1.y <= obj2.y + yUpperProximity)

isCollidingBrickFunction : Brick -> Ball -> Bool
isCollidingBrickFunction brick ball = True

emptyCollidingBrickFunction [] ball = False 

emptyBrickSpecialMultiplierFunction [] ball = 1

emptyCollidingBigballBrickFunction [] ball = ball.bigball

brickSpecialMultiplierFunction : Brick -> Ball -> Float
brickSpecialMultiplierFunction brick ball = if  | brick.slowmo ->
                                                  0.5
                                                | brick.speedup ->
                                                  2.0
                                                | otherwise ->
                                                  1

isCollidingBigBallBrickFunction : Brick -> Ball -> Bool
isCollidingBigBallBrickFunction brick ball = ball.bigball || brick.bigball

countBricks bricks = case bricks of
                              [] -> 0
                              brick::bricks -> 1 + countBricks bricks

updateBricks : Time -> List(Brick) -> Ball -> List(Brick)
updateBricks delta bricks ball = filterBrick ball bricks

physicsUpdate t ({x,y,vx,vy,bigball} as obj) =
  { obj |
      x <- x + vx * t,
      y <- y + vy * t
  }

physicsUpdatePlayer t ({x,y,vx,vy} as obj) =
  { obj |
      x <- x + vx * t,
      y <- y + vy * t
  }

updatePlayer : Time -> Int -> Int -> Player -> Player
updatePlayer t dir points player =
  let
    player_aux =
      Debug.watch "p_aux" (physicsUpdatePlayer  t { player | vx <- toFloat dir * 200 })
  in
    { player_aux |
        x <- clamp (60-halfHeight) (halfHeight-60) player_aux.x,
        score <- player.score + points
    }

filterBrick : Ball -> List(Brick) -> List(Brick)
filterBrick ball bricks = case bricks of
              [] -> []
              brick::bricks -> if (inRange ball brick)
                  then bricks
                  else 
                  brick::filterBrick ball bricks


near punto rango nuevopunto =
  nuevopunto >= punto-rango && nuevopunto <= punto+rango

stepVx vx vy leftCollision rightCollision =
  if  | leftCollision ->
          abs vx
      | rightCollision ->
          -(abs vx)
      | otherwise ->
          vx

stepVy vx vy upperCollision lowerCollision playerCollision brickCollision specialBlock ball =
  if  | playerCollision ->
          -1* vy
      | brickCollision && not ball.slowmo ->
          -1* vy * specialBlock
      | brickCollision ->
          -1* vy
      | upperCollision ->
          abs vy
      | lowerCollision ->
          -(abs vy)
      | otherwise ->
          vy


-- VIEW

view : (Int,Int) -> Game -> Element
view (w,h) {state,ball,player,bricks,level} =
  let
    scores =
      (txt (Text.height 50) ("Desarrollado en Elm"  ))

  in
    container w h middle <|
    collage gameWidth gameHeight
      ([ rect gameWidth gameHeight
          |> filled pong
      , (if | ball.bigball -> oval 30 30
            | otherwise -> oval 15 15)
          |> make ball
      , rect 80 10
          |> make player

      , toForm (if | state == Play -> spacer 1 1
                   | state == Won && level == maxLevel -> txt identity msgWon
                   | state == WonLevel1 -> txt identity msgNextLevel
                   | state == Lost -> txt identity msgLost
                   | otherwise -> txt identity msg)
          |> move (0, 200 - gameHeight/2)
      ]++ (rect 50 10
          |> makeList bricks))


pong =
  rgb 0 0 0


textGreen =
  rgb 160 200 160


txt f string =
  Text.fromString string
    |> Text.color textGreen
    |> Text.monospace
    |> f
    |> leftAligned


msg = "SPACE to start, &larr;&rarr; to move . 
Rojo - slowmotion. 
Azul - pelota grande. 
Verde - pelota rapida"
msgWon = "Won"
msgLost = "Lost"
msgNextLevel = "Next level"

make obj shape =
  shape
    |> filled white
    |> move (obj.x, obj.y)

makeList objlist shape =

 case objlist of
        [] -> []
        o::obj -> (shape
                        |> asignColor o
                        |> move (o.x,o.y)
                  ) :: makeList obj shape

asignColor brick = if | brick.slowmo -> filled red
                      | brick.speedup -> filled green 
                      | brick.bigball -> filled blue
                      | otherwise -> filled white

-- SIGNALS

main =
  Signal.map2 view Window.dimensions gameState


gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input


delta =
  Signal.map inSeconds (fps 35)


input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map3 Input
      Keyboard.space
      (Signal.map .x Keyboard.arrows)
      delta
