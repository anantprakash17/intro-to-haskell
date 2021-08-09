{- Intro to Haskell 1
 - Name: Anant Prakash
 - Date: Aug 8th 2021
 -}
module Task_1 where

factorial :: Integer -> Integer
factorial n = if n > 0
              then n * factorial (n-1)
              else 1

{- -----------------------------------------------------------------
 - cosTaylor
 - -----------------------------------------------------------------
 - Description: Implementation of of the Taylor series function with
                the series going up to order 4. 
 -}
cosTaylor :: Double -> Double -> Double -> Double -> Double
cosTaylor a cos_a sin_a x = let
    step1 = (cos_a) 
    step2 = ((-sin_a) * (x - a)) 
    step3 = (((-cos_a) * ((x - a)**2))/fromIntegral (factorial 2))
    step4 = (((sin_a) * ((x - a)**3))/fromIntegral (factorial 3))
    step5 = (((cos_a) * ((x - a)**4))/fromIntegral (factorial 4))
    in step1 + step2 + step3 + step4 + step5

{- -----------------------------------------------------------------
 - fmod
 - -----------------------------------------------------------------
 - Description: Returns the `mod`/remainder for 2 doubles
 -}
fmod :: Double -> Double -> Double
fmod x y =  ((x / y) -  fromIntegral (floor (x / y))) * y

{- -----------------------------------------------------------------
 - cosApprox
 - -----------------------------------------------------------------
 - Description: Approximates the value of cos(x) using the taylor
 - series above and applying different values of a based on tabluated
 - data provided in the assignment pdf. 
 -}
cosApprox :: Double -> Double
cosApprox x = if (fmod (abs x) (2*pi))  >= 0 && (fmod (abs x) (2*pi) < (pi/4))
                then cosTaylor 0 1 0 (fmod (abs x) (2*pi))
                else if fmod (abs x) (2*pi)  >= (pi/4) && fmod (abs x) (2*pi) < ((3*pi)/4)
                        then cosTaylor (pi/2) 0 1 (fmod (abs x) (2*pi))
                        else if fmod (abs x) (2*pi)  >= ((3*pi)/4) && fmod (abs x) (2*pi) < ((5*pi)/4)
                                then cosTaylor pi (-1) 0 (fmod (abs x) (2*pi))
                                else if fmod (abs x) (2*pi)  >= ((5*pi)/4) && fmod (abs x) (2*pi) < ((7*pi)/4)
                                        then cosTaylor ((3*pi)/2) 0 (-1) (fmod (abs x) (2*pi))
                                        else if fmod (abs x) (2*pi)  >= ((7*pi)/4) && fmod (abs x) (2*pi) < (2*pi)
                                                then cosTaylor (2*pi) 1 0 (fmod (abs x) (2*pi))
                                                else error "Something went wrong"

{- -----------------------------------------------------------------
 - sinApprox
 - -----------------------------------------------------------------
 - Description: uses the equation provided in the assignment 
 - along with the function cosApprox(X) to get an approximate
 - value of Sin(x) using the Talor Approximation of Cos. 
 -}
sinApprox :: Double -> Double
sinApprox x = if not (-(cosApprox (x + (pi/2))) == -0.0)
                then -(cosApprox (x + (pi/2)))
                else 0.0

{- -----------------------------------------------------------------
 - tanApprox
 - -----------------------------------------------------------------
 - Description: Tan(x) = sin(x)/cos(x). This function uses the 
 - approximations of sin and cos from the sinApprox and cosApprox
 - functions to return an approximate value of tan(x). 
 -}
tanApprox :: Double -> Double
tanApprox x = if not ((sinApprox x / cosApprox x) == -0.0)
                then sinApprox x / cosApprox x
                else 0.0

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - Allows an error of 0.01 to compensate for taylor series errors.
 - Tested extreme cases (+ and - 10000000) for all functions.
 - Returned True indicating test succeeded.
 - Cases that output very high values such as tan(pi/2) which is 1.633^16
 - when using prelude tan function returned False in test cases due to Approx 
 - returning infinity.  
 -}
testCos :: Double -> Bool
testCos x = if cosApprox x - cos x < 0.01
                then True
                else False
                
testSin :: Double -> Bool
testSin x = if sinApprox x - sin x < 0.01
                then True
                else False

testTan :: Double -> Bool
testTan x = if tanApprox x - tan x < 0.01
                then True
                else False