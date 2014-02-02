module FpsCounter (fpsCounter, createFpsCounter) where

{-| A frame rate counter chart for your Elm program.

-}

average : [Float] -> Float
average xs = sum xs / toFloat (length xs)

sixty = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,26,28,29,30
        ,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60]

{- Measures fps of the program and creates a real time updating element -}
fpsCounter : Int -> Int -> Signal Element
fpsCounter f n = createFpsCounter f <~ fps 3 `sampleOn` remember n (fps f)

{- Creates a fps counter 

* First argument is frames per second
* Second argument is a list of the time between frames in milliseconds
-}
createFpsCounter : Int -> [Float] -> Element
createFpsCounter f xs = collage (length xs) f 
                      (
                        [traced (solid black) <| path (zipWith (\x y -> (y-30, x - 35)) xs sixty)
                        ]
                      )
                      `beside`
                      plainText (String.left 5 (show (average xs)))

remember t = foldp (\x xs -> 1000 / x :: take (t-1) xs) []
