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

(gameWidth,gameHeight) = (400,400)
(halfWidth,halfHeight) = (200,200)


type State = Play | Pause | Won | Lost


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
  }


player : Float -> Player
player x =
  Player x -150 0 0 0


defaultGame : Game
defaultGame =
  { state = Pause
  , ball = Ball 0 0 200 200 False False False
  , player = player (20 - halfWidth)
  , bricks = [Brick -100 100 1 True False False,
              Brick 0 100 1 True False False, 
              Brick 100 100 1 True False False,
              Brick -100 150 1 True False False, 
              Brick 0 150 1 True False False, 
              Brick 100 150 1 True False False]
  }



type alias Input =
  { space : Bool
  , dir1 : Int
  , delta : Time
  }


-- UPDATE

update : Input -> Game -> Game
update {space,dir1, delta} ({state,ball,player,bricks} as game) =
  let
    score1 = 0
    newState =
      if  | space && state == Play ->
              Pause

          | space && state == Pause ->
              Play

          | 0 == (countBricks bricks) ->
              Won

          | (ball.y < 7-halfHeight) ->
              Lost

          | otherwise ->
              state
    newBall =
      if state == Pause || state == Won || state == Lost then
        ball
      else
        updateBall delta ball player bricks
    newBricks = updateBricks delta bricks ball
  in
    { game |
        state <- newState,
        ball <- newBall,
        player <- updatePlayer delta dir1 score1 player,
        bricks <- newBricks
    }



updateBall : Time -> Ball -> Player -> List(Brick)-> Ball
updateBall t ({x,y,vx,vy} as ball) p1 bricks =
  if not (ball.x |> near 0 halfWidth) then
    { ball | x <- 0, y <- 0 }
  else
    physicsUpdate t
      { ball |
          vx <- stepVx vx vy (x < 7-halfWidth)(x > halfWidth-7),
          vy <- stepVy vx vy (y < 7-halfHeight) (y > halfHeight-7)
                   ( x >= p1.x - 40 
                  && x <= p1.x + 40 
                  && y >= -155 
                  && y <= -145)
                (brickCollision bricks ball isCollidingBrickFunction emptyCollidingBrickFunction)
                (brickCollision bricks ball brickSpecialMultiplierFunction emptyBrickSpecialMultiplierFunction)
                ball
          slowmo <-
          speedup <-
          bigball <-
      }


brickCollision bricks ball function empty = 
case bricks of
    [] -> empty []
    brick::bricks -> if (inRange ball brick)
                        then function brick
                     else 
                        brickCollision bricks ball function empty
                        
inRange ball brick = (ball.x >= brick.x - 40 
                   && ball.x<= brick.x + 40 
                   && ball.y >= brick.y - 40 
                   && brick.y <= ball.y + 40)

isCollidingBrickFunction : Brick -> Bool
isCollidingBrickFunction brick = True

emptyCollidingBrickFunction [] = False 

emptyBrickSpecialMultiplierFunction [] = 1

brickSpecialMultiplierFunction : Brick -> Float
brickSpecialMultiplierFunction brick = if  | brick.slowmo ->
                                              0.7
                                           | brick.speedup ->
                                              1.2
                                           | otherwise ->
                                              1

countBricks bricks = case bricks of
                              [] -> 0
                              brick::bricks -> 1 + countBricks bricks

updateBricks : Time -> List(Brick) -> Ball -> List(Brick)
updateBricks delta bricks ball = filterBrick ball bricks


physicsUpdate t ({x,y,vx,vy} as obj) =
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
        x <- clamp (22-halfHeight) (halfHeight-22) player_aux.x,
        score <- player.score + points
    }

filterBrick : Ball -> List(Brick) -> List(Brick)
filterBrick ball bricks = case bricks of
              [] -> []
              brick::bricks -> if (near brick.x 40 ball.x && near brick.y 40 ball.y)
                  then bricks
                  else 
                  brick::filterBrick ball bricks


near k c n =
  n >= k-c && n <= k+c

within ball paddle =
  near paddle.x 80 ball.x && near paddle.y 40 ball.y

stepVx vx vy leftCollision rightCollision =
  if  | leftCollision ->
          abs vx
      | rightCollision ->
          -(abs vx)
      | otherwise ->
          vx

stepVy vx vy leftCollision rightCollision playerCollision brickCollision specialBlock ball =
  if  | playerCollision ->
          -1* vy
      | brickCollision && not ball.slowMotion ->
          -1* vy * specialBlock
      | brickCollision ->
          -1* vy
      | leftCollision ->
          abs vy
      | rightCollision ->
          -(abs vy)
      | otherwise ->
          vy


-- VIEW

view : (Int,Int) -> Game -> Element
view (w,h) {state,ball,player,bricks} =
  let
    scores =
      (txt (Text.height 50) ("Desarrollado en Elm"  ))

  in
    container w h middle <|
    collage gameWidth gameHeight
      ([ rect gameWidth gameHeight
          |> filled pongGreen
      , oval 15 15
          |> make ball
      , rect 80 10
          |> make player

      , toForm (if state == Play then spacer 1 1
                else if state == Won then
                  txt identity msgWon
                else if state == Lost then
                  txt identity msgLost
                else
                  txt identity msg)
          |> move (0, 40 - gameHeight/2)
      ]++ (rect 50 10
          |> makeList bricks))


pongGreen =
  rgb 0 0 0


textGreen =
  rgb 160 200 160


txt f string =
  Text.fromString string
    |> Text.color textGreen
    |> Text.monospace
    |> f
    |> leftAligned


msg = "SPACE to start, &larr;&rarr; to move"
msgWon = "Won"
msgLost = "Lost"

make obj shape =
  shape
    |> filled white
    |> move (obj.x, obj.y)

makeList objlist shape =

 case objlist of
        [] -> []
        o::obj -> (shape
                        |> filled blue
                        |> move (o.x,o.y)
                  ) :: makeList obj shape


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
