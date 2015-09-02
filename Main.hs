import Const
import Graphics.Gloss

window :: Display
window = InWindow "Snake" (windowWidth, windowHeight) (0, 0)

--Shifting origin to bottom left corner
translateHeight :: Float
translateHeight = fromIntegral (side - windowHeight)/2

translateWidth :: Float
translateWidth = fromIntegral (side - windowWidth)/2

pixel = translate translateWidth translateHeight $ rectangleSolid sideF sideF

main :: IO ()
main = display window white pixel
