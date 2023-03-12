-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest2 (stateTrib, runStateTrib, writeLeaves, collapse, mapLeavesWithAddress, toQuadTree, fromQuadTree) where

import Data.List
import Data.Char

import Control.Monad.State
import Control.Monad.Writer

import Types

-- Question 1

stateTrib :: Integer -> State (Integer,Integer,Integer) ()
stateTrib (-1) = pure ()
stateTrib 0 = pure ()
stateTrib 1 = pure ()
stateTrib n = do
                modify (\(z,y,x) -> (x+y+z,z,y))
                stateTrib (n-1)


runStateTrib :: Integer -> Integer
runStateTrib n =
  let ((),(a,b,c)) = runState (stateTrib n) (1,0,0)
  in a

-- Question 2

writeLeaves :: Bin a b -> Writer [Either a b] ()
writeLeaves (Lf a) = tell [Left a]
writeLeaves (Nd b l r) = do 
                            writeLeaves l
                            tell [Right b]
                            writeLeaves r

-- Question 3

collapse :: Bin (Bin a b) b -> Bin a b
--collapse (Bin(Lf a))  = collapse a
--collapse (Nd x l r) = Nd x (Lf l) (Lf r)
--collapse = undefined
--Doesnt work
--RIP

collapse (Nd x l r) = Nd x (collapse l) (collapse r)
collapse (Lf (Nd x l r)) = (Nd x l r)
collapse (Lf (Lf x)) = (Lf x) --For exhaustive 

-- Question 4

mapLeavesWithAddress :: (a -> Address -> c) -> Bin a b -> Bin c b
--mapLeavesWithAddress f (Nd x l r) = Nd x ((mapLeavesWithAddress f l)) ((mapLeavesWithAddress f r))
--mapLeavesWithAddress f (Lf leaf) = (Lf leaf)
mapLeavesWithAddress = undefined



-- Question 5
toQuadTree :: Image -> QuadTree
toQuadTree (image:[]) = P (image !! 0)
toQuadTree (image) = N (toQuadTree (topL)) (toQuadTree (topR)) (toQuadTree (bottomL)) (toQuadTree (bottomR))
                         where topL =  [take lengthDivTwo pixelSing | pixelSing <- image, case elemIndex pixelSing image of
                                                      Just pixelSing -> if (Just pixelSing < Just lengthDivTwo) then True else False
                                                      Nothing -> False]
                               topR = [reverse $ take lengthDivTwo $ reverse pixelSing | pixelSing <- image, 
                                                    case elemIndex pixelSing image of
                                                      Just pixelSing -> if (Just pixelSing < Just lengthDivTwo) then True else False
                                                      Nothing -> False]   
                               bottomL = [take lengthDivTwo pixelSing | pixelSing <- image, 
                                                    case elemIndex pixelSing image of
                                                       Just pixelSing -> if (Just pixelSing >= Just lengthDivTwo) then True else False
                                                       Nothing -> False] 
                               bottomR = [reverse $ take lengthDivTwo $ reverse pixelSing | pixelSing <- image, 
                                                    case elemIndex pixelSing image of 
                                                       Just pixelSing -> if (Just pixelSing >= Just lengthDivTwo) then True else False
                                                       Nothing -> False]
                               lengthDivTwo = ((length image) `div` 2)
                               
fromQuadTree :: QuadTree -> Image
fromQuadTree = undefined