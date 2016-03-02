module Page where

import Random
import Signal
import Graphics.Element exposing (..)
import String
import Window
import Color exposing (..)
import Graphics.Collage exposing (..)
import Time exposing (every, millisecond)
import Text exposing (..)
  -- TODO: modify/add imports as needed

type alias Point = { x:Float, y:Float }

type alias State = ((Int, List Point), (Int, List Point))

initState = ((0,[]), (0,[]))

distance : Point -> Float
distance pt = 
  sqrt (pt.x^2 + pt.y^2)


upstate : Point -> State -> State
upstate pt st = 
  case st of
  ((hits, lHits), (nohits, lNoHits)) ->
    if (distance pt) <= 0.5 then ((hits + 1, pt :: lHits), (nohits, lNoHits)) else
    ((hits, lHits), (nohits + 1, pt :: lNoHits))
  
height = 200
width = 200

pointsToSquare : Color.Color -> List Point -> List Form
pointsToSquare c lps = 
  case lps of 
    [] -> []
    pt::tail -> (move ((width*pt.x), (height*pt.y)) (filled c (circle 10))) :: (pointsToSquare c tail)

piApprox : State -> Float
piApprox s = 
  case s of 
    ((hits, lHits), (nohits, lNoHits)) ->
      4.0 * (toFloat nohits) / (toFloat (hits + nohits))


stateAppend : State -> List Form
stateAppend st = 
  case st of
    ((hits, lHits), (nohits, lNoHits)) -> 
      (pointsToSquare yellow lHits) ++ (pointsToSquare black lNoHits)

showPi : Float -> Float -> Form
showPi y pi = 
  moveY y (toForm (show pi))

view : (Int,Int) -> State -> Element
view (w,h) st =
  collage w h (background (w,h) st)

background : (Int, Int) -> State -> List Form
background (w,h) st =
    [toForm (image w h ("img/personalBackground.png")),
    toForm (centered (Text.link "samsegan.github.io" (fromString "samsegan.github.io"))) |> move (toFloat -w/3, toFloat h/4),
    toForm (centered (Text.link "desolate-headland-7179.herokuapp.com" (fromString "desolate-headland-7179.herokuapp.com"))) |> move (toFloat w/3,toFloat h/4),
    toForm (show (piApprox st)) |> moveY -250] ++ 
    stateAppend st


genPoint : Random.Seed -> (Point, Random.Seed)
genPoint s =
  case (Random.generate (Random.pair (Random.float -1 1) (Random.float -1 1)) s) of
  ((a1, a2), seed) -> ({x = a1, y = a2}, seed)

signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
  Signal.foldp (\p ss -> genPoint(snd(ss))) ({x=0, y=0}, Random.initialSeed 0) (every millisecond) 
  --Signal.constant (genPoint (Random.initialSeed 0))

signalPoint : Signal Point
signalPoint =
  Signal.map fst (signalPointSeed)
 -- Signal.constant {x=0,y=0}

main : Signal Element
main =
  Signal.map2 view Window.dimensions
    (Signal.foldp upstate initState signalPoint)



