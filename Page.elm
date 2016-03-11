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
  
height = 100
width = 100

pointsToSquare : (Int, Int) -> Color.Color -> List Point -> List Form
pointsToSquare (w,h) c lps = 
  case lps of 
    [] -> []
    pt::tail -> (move (((toFloat w)*pt.x), ((toFloat h)*pt.y)) (filled c (circle 1))) :: (pointsToSquare (w,h) c tail)

piApprox : State -> Float
piApprox s = 
  case s of 
    ((hits, lHits), (nohits, lNoHits)) ->
      4.0 * (toFloat nohits) / (toFloat (hits + nohits))


stateAppend : (Int, Int) -> State -> List Form
stateAppend (w,h) st = 
  case st of
    ((hits, lHits), (nohits, lNoHits)) -> 
      (pointsToSquare (w,h) white lHits) ++ (pointsToSquare (w,h) black lNoHits)

showPi : Float -> Float -> Form
showPi y pi = 
  moveY y (toForm (show pi))

view : (Int,Int) -> State -> Element
view (w,h) st =
  collage w h (background (w,h) st)
  --(centered (Text.link "samsegan.github.io" (fromString "samsegan.github.io")))

gameDesc : String
gameDesc = 
  "This is a link to my recreation of the popular\npuzzle game, 2048. This was created, along\n
  with this page, using the Elm functional programming language."

webAppDesc : String
webAppDesc = 
  "Here is the web application that I built to ease the process of sourcing private companies
  for potential investment targets. It contains a database of hundreds of thousands of companies, 
  tracks the growth of these companies, and lets the user track individual investment targets."

lineText : (Float, Float) -> String -> Form
lineText (x,y) s =
  (text (bold (Text.color white (fromString s)))) |> move (x,y)

background : (Int, Int) -> State -> List Form
background (w,h) st =
    [toForm (image w h ("img/uchicagoColor.png")),
    toForm (image 500 130 ("img/PersonalLogo.png")) |> move (0,280),
    (text (bold (Text.color white (Text.height 20 (fromString "Pi: Now being calculated by the position of the white and black points on this screen"))))) |> move (0,toFloat (-h//2 +40)),
    (text (bold (Text.color white (fromString "Everything on this page is created by Sam Segan, 2016")))) |> move(0,-300),
    toForm (image 200 25 ("img/linkBackground.png")) |> move (0,toFloat (-h//2 +10)),
    toForm (show (piApprox st)) |> move (0,toFloat (-h//2 +10))] ++ 
    stateAppend (w,h) st ++ 
      [
      --(text (bold (Text.color white (Text.height 20 (fromString "About"))))),
      --toForm (image 700 120 ("img/textBack.png")) |> moveY -110,
      (text (bold (Text.color white (Text.height 20 (fromString "I am a junior at the University of Chicago"))))) |> move (0,-50),
      (text (bold (Text.color white (Text.height 20 (fromString "majoring in Computer Science and Economics."))))) |> move (0,-75),
      (text (bold (Text.color white (Text.height 20 (fromString "I love computers, and love to build things."))))) |> move (0,-100),
      (text (bold (Text.color white (Text.height 20 (fromString "I look for challenges and am passionate about solving difficult problems."))))) |> move (0,-125),
      --lineText (0,-15) "I am a junior at the University of Chicago majoring in Computer Science and Economics. ",
      --lineText (0,-30) "I love computers, and love to build things. I look for challenges and am passionate about solving difficult problems.",
      
      toForm (image 385 130 ("img/textBox.png")) |> move (toFloat -w/3, toFloat h/4 - 65),
      lineText (toFloat -w/3, toFloat h/4 - 25) "This is a link to my recreation of the popular puzzle",
      lineText (toFloat -w/3, toFloat h/4 - 45) "game, 2048. This was created, along with the page you are",
      lineText (toFloat -w/3, toFloat h/4 - 65) "looking at, using the Elm functional programming language.",

      toForm (image 390 130 ("img/textBox.png")) |> move (toFloat w/3, toFloat h/4 - 65),
      lineText (toFloat w/3, toFloat h/4 - 25) "I built the web application above to ease the",
      lineText (toFloat w/3, toFloat h/4 - 45) "process of sourcing private companies for potential",
      lineText (toFloat w/3, toFloat h/4 - 65) "investment targets. It contains a database of hundreds",
      lineText (toFloat w/3, toFloat h/4 - 85) "of thousands of companies, tracks the growth of these",
      lineText (toFloat w/3, toFloat h/4 - 105) "companies, and lets the user track individual investment targets.",

      toForm (image 150 150 ("img/uchicago.png")) |> move (toFloat (-w//2 + 100),toFloat (-h//2 + 100)),
      toForm (image 140 65 ("img/2048Logo.png")) |> move (toFloat -w/3, toFloat h/4 + 65),
      --toForm (image 500 100 ("img/webAppPic.png")) |> move (toFloat w/3, toFloat h/4 + 65),
      toForm (image 50 20 ("img/linkBackground.png")) |> move (toFloat -w/3, toFloat h/4 + 20),
      toForm (image 100 20 ("img/linkBackground.png")) |> move (toFloat w/3,toFloat h/4 + 20),
      toForm (centered (Text.link "https://samsegan.github.io" (fromString "2048"))) |> move (toFloat -w/3, toFloat h/4 + 20),
      toForm (centered (Text.link "https://desolate-headland-7179.herokuapp.com/" (fromString "Sourcing Tool"))) |> move (toFloat w/3,toFloat h/4 + 20)]

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



