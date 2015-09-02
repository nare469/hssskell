import Const
import Graphics.Gloss

--data Definitions
data Pixel = Pixel Int Int

translatePixel :: Int -> Int -> Pixel -> Pixel
translatePixel dx dy (Pixel x y) = Pixel (x+dx) (y+dy)

type Snake = [Pixel]

data Game = Game { snakePos :: Snake }
initialState :: Game 
initialState = Game {snakePos = [Pixel (div wBlocks 2) (div hBlocks 2)] }

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
renderState = pictures . drawSnake . snakePos

main :: IO ()
main = display window white $ renderState initialState
