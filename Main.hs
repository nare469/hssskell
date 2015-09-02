import Const
import Data
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

translatePixel :: Int -> Int -> Pixel -> Pixel
translatePixel dx dy (Pixel x y) = Pixel (x+dx) (y+dy)

initialState :: Game 
initialState = Game
    { snake = [
        Pixel (div wBlocks 2) (div hBlocks 2),
        Pixel ((div wBlocks 2) - 1) (div hBlocks 2),
        Pixel ((div wBlocks 2) - 2) (div hBlocks 2) ]
    , direction = Data.Right }

--Shifting origin to bottom left corner
translateHeight :: Float
translateHeight = fromIntegral (side - windowHeight)/2

translateWidth :: Float
translateWidth = fromIntegral (side - windowWidth)/2

--rendering the data
drawPixelOrigin :: Picture
drawPixelOrigin = translate translateWidth translateHeight $ rectangleSolid sideF sideF

drawPixel :: Pixel -> Picture
drawPixel (Pixel x y) = translate (fromIntegral $ side*x) (fromIntegral $ side*y) drawPixelOrigin

drawSnake :: Snake -> [Picture]
drawSnake = map drawPixel

window :: Display
window = InWindow "Snake" (windowWidth, windowHeight) (0, 0)

renderState :: Game -> Picture
renderState = pictures . drawSnake . snake

--Other functions
moveSnake :: Snake -> Direction -> Snake
moveSnake s d
    | d == Data.Up    = (translatePixel 0 1 $ head s):(init s)
    | d == Data.Down  = (translatePixel 0 (-1) $ head s):(init s)
    | d == Data.Right = (translatePixel 1 0 $ head s):(init s)
    | d == Data.Left  = (translatePixel (-1) 0 $ head s):(init s)

update :: Float -> Game -> Game
update _ g = g { snake = (moveSnake (snake g) (direction g)) }

keyPressHandler :: Event -> Game -> Game
keyPressHandler (EventKey (Char 's') _ _ _) game = game { direction = Data.Down }
keyPressHandler (EventKey (Char 'w') _ _ _) game = game { direction = Data.Up }
keyPressHandler (EventKey (Char 'a') _ _ _) game = game { direction = Data.Left }
keyPressHandler (EventKey (Char 'd') _ _ _) game = game { direction = Data.Right }
keyPressHandler _ game = game

main :: IO ()
main = play window white 1 initialState renderState keyPressHandler update
