doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

doubleSmallNumber' x = ( if x > 100 then x else x * 2 ) + 1

conanO'Brein = "It's a-me, Conan O'Brein!"

lostNumbers = [ 4, 8, 15, 16, 23, 42 ]

nouns = [ "hobo", "frog", "pope" ]

adjectives = [ "lazy", "grouchy", "scheming" ]

length' xs = sum [ 1 | _ <- xs ]

rightTriangles' = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a + b + c == 24 ]

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial ( n - 1 )

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

addVectors :: ( Double, Double ) -> ( Double, Double ) -> ( Double, Double )
addVectors ( x1, y1 ) ( x2, y2 ) = ( x1 + x2, y1 + y2 )

first :: ( a, b, c ) -> a
first ( x, _, _ ) = x

second :: ( a, b, c ) -> b
second ( _, y, _ ) = y

third :: ( a, b, c ) -> c
third ( _, _, z ) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' ( x : _ ) = x

tell :: ( Show a ) => [a] -> String
tell [] = "The list is empty"
tell ( x : [] ) = "The list has one element: " ++ show x
tell ( x : y : [] ) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell ( x : y : _ ) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@( x : xs ) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal.\
                   \ Pfft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2

badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

niceGreeting :: String
niceGreeting = "Hello! So very nicwe to see you."

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Frenando" = niceGreeting ++ " Frenando!"
greet name = badGreeting ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where ( f : _ ) = firstname
        ( l : _ ) = lastname

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of []  -> "empty"
                                               [x] -> "a singleton list."
                                               xs  -> "a longer list."
