import Const
import Graphics.Gloss

window :: Display
window = InWindow "Snake" (windowWidth*side, windowHeight*side) (0, 0)

main :: IO ()
main = display window white (rectangleSolid sideF sideF)
