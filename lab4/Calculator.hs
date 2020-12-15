-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI

import Expr

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

scale = 20.0 / canWidth

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     diff    <- mkButton "diff"  -- The diff button
     zoom    <- mkSlider (1, 10) 1  -- The diff button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw,pure zoom,pure diff]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input zoom canvas
     on UI.click     diff  $ \ _ -> diffAndDraw input zoom canvas
     on valueChange' input $ \ _ -> readAndDraw input zoom canvas
     on valueChange' zoom $ \ _ -> readAndDraw input zoom canvas


readAndDraw :: Element -> Element -> Canvas -> UI ()
readAndDraw input zoom canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     zoomV <- fromIntegral . read <$> get value zoom
     -- Clear the canvas
     clearCanvas canvas

     case readExpr formula of
         Nothing -> return ()
         Just expr -> do
             let pointsList = points expr (scale*zoomV) (canWidth, canHeight)

             -- The following code draws the formula text in the canvas and a blue line.
             -- It should be replaced with code that draws the graph of the function.
             set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
             UI.fillText formula (10,canHeight/2) canvas
             path "blue" pointsList canvas

--Part II-----------------------
--H-----------------------------

points :: Expr -> Double -> (Int,Int) -> [Point]
points expr scale (width, height) =
        [(x, realToPix (eval expr (pixToReal x)) ) | x <- [0.. fromIntegral width]]
  where
    -- converts a pixel x-coordinate to a real x-coordinate
    pixToReal :: Double -> Double
    pixToReal x = scale * (x - fromIntegral width)

    -- converts a real y-coordinate to a pixel y-coordinate
    realToPix :: Double -> Double
    --realToPix = (+) (fromIntegral height/2.0) . (/scale)
    realToPix = (-)  (fromIntegral height/2.0) . (/scale)


--K-----------------------------

diffAndDraw :: Element -> Element -> Canvas -> UI ()
diffAndDraw input zoom canvas =
  do -- Get the current formula (a String) from the input element
     readAndDraw input zoom canvas

     formula <- get value input
     zoomV <- fromIntegral . read <$> get value zoom

     case readExpr formula of
         Nothing -> return ()
         Just expr -> do
             let diff = differentiate expr
             let pointsList = points diff (scale*zoomV) (canWidth, canHeight)

             let diffS = show diff
             UI.fillText diffS (10,canHeight/2+10) canvas
             path "Red" pointsList canvas
             pure input # set value diffS
             return ()
