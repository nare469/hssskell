import Prelude hiding (Left, Right)
import Const
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

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
    | d == Main.Up    = (translatePixel 0 1 $ head s):(init s)
    | d == Main.Down  = (translatePixel 0 (-1) $ head s):(init s)
    | d == Main.Right = (translatePixel 1 0 $ head s):(init s)
    | d == Main.Left  = (translatePixel (-1) 0 $ head s):(init s)

update :: Float -> Game -> Game
update _ g = g { snake = (moveSnake (snake g) (direction g)) }

keyPressHandler :: Event -> Game -> Game
keyPressHandler (EventKey (Char 's') _ _ _) game = game { direction = Main.Down }
keyPressHandler (EventKey (Char 'w') _ _ _) game = game { direction = Main.Up }
keyPressHandler (EventKey (Char 'a') _ _ _) game = game { direction = Main.Left }
keyPressHandler (EventKey (Char 'd') _ _ _) game = game { direction = Main.Right }
keyPressHandler _ game = game

main :: IO ()
main = play window white 1 initialState renderState keyPressHandler update
