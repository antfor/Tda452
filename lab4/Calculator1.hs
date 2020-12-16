-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI

import Data.Maybe
import Expr

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     scaleInput <- mkInput 10 "Scaling"
     scale   <- mkButton "Scale Graph"
     diff    <- mkButton "Differentiate"
     zoom      <- mkHTML "Zoom: 1.0"
     slide <- mkSlider (1,60) 10
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw, pure diff, pure zoom, pure slide]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]
     path "grey" [(0,canHeight/2),(canWidth,canHeight/2)] canvas
     path "grey" [(canWidth/2,0),(canWidth/2,canHeight)] canvas
     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input slide canvas
     --on UI.click     scale  $ \ _ -> scaleAndDraw input scaleInput canvas
     on UI.click     diff  $ \ _ -> diffAndDraw input slide canvas
     --on valueChange' input $ \ _ -> readAndDraw input canvas
     on valueChange' slide $ \ _ -> zoomAndDraw input zoom slide canvas

-- I ------------------------
readAndDraw :: Element -> Element -> Canvas -> UI ()
readAndDraw input slider canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     slider <- get value slider
     let sl = 10/(fst $ head (reads slider :: [(Double,String)]))
     draw' (fromJust $ readExpr formula) sl canvas

draw' :: Expr -> Double -> Canvas -> UI ()
draw' expr zoom canvas = do
  -- Clear the canvas
  clearCanvas canvas
  path "grey" [(0,canHeight/2),(canWidth,canHeight/2)] canvas
  path "grey" [(canWidth/2,0),(canWidth/2,canHeight)] canvas
  path "blue" (points expr (zoom*0.04) (canWidth, canHeight)) canvas

-- H ------------------------
points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width,height) = [((fromIntegral x),realToPix (yPos x)) | x <- [0..width]]
                where
                  yPos x = eval exp (pixToReal (fromIntegral x))
                  pixToReal :: Double -> Double
                  pixToReal x = scale*((x-fromIntegral ((width `div ` 2))))

                  realToPix :: Double -> Double
                  realToPix y = (-(y/scale)+(fromIntegral(height `div` 2)))


-- J ------------------------
zoomAndDraw :: Element -> Element -> Element -> Canvas -> UI ()
zoomAndDraw input zoom slider canvas =
  do -- Get the current formula (a String) from the input element
    formula <- get value input
    slider <- get value slider
    let sl = (fst $ head (reads slider :: [(Double,String)]))
    element zoom # set UI.html ("Zoom: " ++ (show (sl/10)))

    draw' (fromJust $ readExpr formula) (10/sl) canvas

-- K -------------------------
diffAndDraw :: Element -> Element -> Canvas -> UI ()
diffAndDraw input slide canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     let expr = differentiate $ fromJust $ readExpr formula
     element input # set UI.value (showExpr expr)
     readAndDraw input slide canvas
