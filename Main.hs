import Prelude hiding (Left, Right)
import Const
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

--data Definitions
data Pixel = Pixel Int Int

translatePixel :: Int -> Int -> Pixel -> Pixel
translatePixel dx dy (Pixel x y) = Pixel (x+dx) (y+dy)

type Snake = [Pixel]

data Game = Game 
    { snake :: Snake
    , direction :: Direction }

initialState :: Game 
initialState = Game
    { snake = [Pixel (div wBlocks 2) (div hBlocks 2)]
    , direction = Right }

data Direction = Right | Left | Up | Down
    deriving (Eq)

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
    | d == Up    = (translatePixel 0 1 $ head s):(init s)
    | d == Down  = (translatePixel 0 (-1) $ head s):(init s)
    | d == Right = (translatePixel 1 0 $ head s):(init s)
    | d == Left  = (translatePixel (-1) 0 $ head s):(init s)

update :: ViewPort -> Float -> Game -> Game
update _ _ g = g { snake = (moveSnake (snake g) (direction g)) }

main :: IO ()
main = simulate window white 1 initialState renderState update 
