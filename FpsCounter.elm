module FpsCounter (fpsCounter, createFpsCounter) where

{-| A frame rate counter chart for your Elm program.

@docs fpsCounter, createFpsCounter
-}

average : [Float] -> Float
average xs = sum xs / toFloat (length xs)

sixty = [1..60]

{-| Measures fps of the program and creates a real time updating element -}
fpsCounter : Int -> Int -> Signal Element
fpsCounter f n = createFpsCounter f <~ fps 3 `sampleOn` remember n (fps f)

{-| Creates a fps counter

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
