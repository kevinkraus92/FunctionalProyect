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
(maxLevel) = (5)

type State = Play | Pause | Won | Lost | WonLevel

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
  , bigpad : Bool
  }

type alias Brick =
  { x : Float
  , y : Float
  , active : Int
  , slowmo: Bool
  , bigball: Bool
  , speedup: Bool
  , bigpad: Bool
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
  Player x -250 0 0 0 False

defaultGame : Game
defaultGame =
  { state = Pause
  , ball = initialBallPosition
  , player = player 0
  , bricks = restartBricks
  , level = startLevel
  }

startLevel = 1

initialBallPosition = Ball 0 0 200 200 False False False
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
      if  | space && state == Play -> Pause
          | space && state == Pause -> Play
          | 0 == (countBricks bricks) && level < maxLevel -> WonLevel
          | 0 == (countBricks bricks) && level == maxLevel -> Won
          | space && state == Lost -> Play
          | (ball.y < -280) -> Lost
          | otherwise -> state

    newLevel = if | newStateMoment == WonLevel -> (level + 1)
                  | newStateMoment == Lost -> 1
                  | otherwise -> level

    newBall = if | state == Lost -> initialBallPosition
                 | state == Pause || state == Won -> ball
                 | otherwise -> updateBall delta ball player bricks

    newBricks = if | newStateMoment == WonLevel -> newLevelSelector newLevel
                   | newStateMoment == Lost -> restartBricks
                   | otherwise -> updateBricks delta bricks ball

    newState = if | newStateMoment == WonLevel -> Play
                  | otherwise -> newStateMoment
  in
  let
    game_aux =
      Debug.watch "game_aux" (newLevel)
  in
    { game |
        state <- newState,
        ball <- newBall,
        player <- updatePlayer delta dir1 score1 player bricks ball,
        bricks <- newBricks,
        level <- newLevel
    }

newLevelSelector level = if | level == 2 -> level2Bricks
                            | level == 3 -> level3Bricks
                            | level == 4 -> level4Bricks
                            | level == 5 -> level5Bricks
                            | otherwise -> restartBricks

level4Bricks = [Brick -100 200 1 False False True False,
              Brick -100 125 1 False True False False,
              Brick -100 75 1 False False True False,
              Brick -200 100 1 True False False False,
              Brick 0 100 1 False False True False,
              Brick 100 200 1 False False True False,
              Brick 100 125 1 False True False False,
              Brick 100 75 1 False False True False,
              Brick 200 100 1 True False False False
              ]

level3Bricks = [Brick -200 250 1 False False True False,
              Brick -150 250 1 False False False False,
              Brick -100 250 1 False True False False,
              Brick -50 250 1 False False False False,
              Brick 0 250 1 False False False True,
              Brick 50 250 1 False True False False,
              Brick 100 250 1 False False True False,
              Brick 150 250 1 False True False False,
              Brick 200 250 1 True False False False,
              Brick -200 150 1 False False True False,
              Brick -150 150 1 False False False False,
              Brick -100 150 1 False True False False,
              Brick -50 150 1 False False False False,
              Brick 0 150 1 False False False True,
              Brick 50 150 1 False True False False,
              Brick 100 150 1 False False True False,
              Brick 150 150 1 False True False False,
              Brick 200 150 1 True False False False,
              Brick -200 200 1 False False True False,
              Brick -150 200 1 False False False False,
              Brick -100 200 1 False True False False,
              Brick -50 200 1 False False False False,
              Brick 0 200 1 False False False True,
              Brick 50 200 1 False True False False,
              Brick 100 200 1 False False True False,
              Brick 150 200 1 False True False False,
              Brick 200 200 1 True False False False
              ]

level5Bricks = [Brick -100 200 1 False False True False,
              Brick -100 125 1 False False False False,
              Brick -100 75 1 False False False False,
              Brick -200 100 1 False False False False,
              Brick 0 100 1 False False True False
              ]

level2Bricks = [Brick -100 100 1 True False False False,
              Brick -100 125 1 False False False False,
              Brick -100 75 1 False False False False,
              Brick -200 100 1 False False False False,
              Brick 0 100 1 False False True False, 
              Brick 0 125 1 False False False False, 
              Brick 0 150 1 False False False False, 
              Brick 0 75 1 False False False False, 
              Brick 0 50 1 False False False False, 
              Brick 100 100 1 False True False False,
              Brick 100 125 1 False False False False,
              Brick 100 75 1 False False False False,
              Brick 200 100 1 False False False False
              ]
restartBricks = [
      Brick -200 100 1 False True False False,
      Brick -100 100 1 False False False False,
      Brick 0 100 1 False False False False, 
      Brick 100 100 1 False False False False,
      Brick 200 100 1 False False False False,
      Brick -200 250 1 False False False True, 
      Brick -100 250 1 False False True False, 
      Brick 0 250 1 True False False False, 
      Brick 100 250 1 False False False False,
      Brick 200 250 1 False False False True
  ]

updateBall : Time -> Ball -> Player -> List(Brick)-> Ball
updateBall t ({x,y,vx,vy, slowmo, bigball} as ball) p1 bricks =
  if not (ball.x |> near 0 halfWidth) then
    { ball | x <- 0, y <- 0 }
  else
    physicsUpdate t
      { ball |
          bigball <- (brickCollision bricks ball p1 isCollidingBigBallBrickFunction emptyCollidingBigballBrickFunction),
          vx <- stepVx vx vy (collision x (7-halfWidth))(collision (halfWidth-7) x),
          vy <- stepVy vx vy (collision y (7-halfHeight)) (collision (halfHeight-7) y)
                (playerCollision x y p1)
                (brickCollision bricks ball p1 isCollidingBrickFunction emptyCollidingBrickFunction)
                (brickCollision bricks ball p1 brickSpecialMultiplierFunction emptyBrickSpecialMultiplierFunction) 
                ball
      }

collision a b = a < b
playerCollision x y p1 = ( x >= p1.x + xLeftProximity 
                  && x <= p1.x + xRightProximity 
                  && y >= p1.y - 10 
                  && y <= p1.y + 10)
brickCollision bricks ball player function empty = case bricks of  
                                              [] -> empty [] ball player
                                              brick::bricks -> if (inRange ball brick)
                                                                  then function brick ball player
                                                               else 
                                                               brickCollision bricks ball player function empty

inRange obj1 obj2 = (obj1.x >= obj2.x + xLeftProximity 
                   && obj1.x <= obj2.x + xRightProximity 
                   && obj1.y >= obj2.y + yLowerProximity 
                   && obj1.y <= obj2.y + yUpperProximity)

isCollidingBrickFunction : Brick -> Ball -> Player -> Bool
isCollidingBrickFunction brick ball player = True

emptyCollidingBrickFunction [] ball player = False 

emptyBrickSpecialMultiplierFunction [] ball player = 1

emptyCollidingBigballBrickFunction [] ball player = ball.bigball

brickSpecialMultiplierFunction : Brick -> Ball -> Player -> Float
brickSpecialMultiplierFunction brick ball player = if  | brick.slowmo ->
                                                          0.5
                                                       | brick.speedup ->
                                                          2.0
                                                       | otherwise ->
                                                          1

isCollidingBigBallBrickFunction : Brick -> Ball -> Player -> Bool
isCollidingBigBallBrickFunction brick ball player = ball.bigball || brick.bigball

emptyBigPadFunction [] ball player = player.bigpad
bigPadFunction brick ball player = brick.bigpad

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

updatePlayer : Time -> Int -> Int -> Player -> List(Brick) -> Ball -> Player
updatePlayer t dir points player bricks ball =
  let
    player_aux =
      Debug.watch "p_aux" (physicsUpdatePlayer  t { player | vx <- toFloat dir * 200 })
  in
    { player_aux |
        x <- clamp (60-halfHeight) (halfHeight-60) player_aux.x,
        score <- player.score + points,
        bigpad <- (brickCollision bricks ball player bigPadFunction emptyBigPadFunction) 
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
      , (if | player.bigpad -> rect 120 10
            | otherwise -> rect 80 10)
          |> make player

      , toForm (if | state == Play -> spacer 1 1
                   | state == Won && level == maxLevel -> txt identity msgWon
                   | state == WonLevel -> txt identity msgNextLevel
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
Verde - pelota rapida
Amarillo - paleta grande"
msgWon = "Won"
msgLost = "Lost. Press SPACE to restart"
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
                      | brick.bigpad -> filled yellow
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
