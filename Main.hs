import Const
import Graphics.Gloss

window :: Display
window = InWindow "Snake" (15*squareSide, 15*squareSide) (10, 10)

main :: IO ()
main = display window white (circle 10)
