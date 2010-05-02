{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Groups
import Data.List (elemIndex, nub, intersperse, intercalate, sort, (\\))

type Cycle a = [a]
data Permutation a = Perm [a] [Cycle a] -- deriving Show

pDomain :: Permutation a -> [a]
pDomain (Perm d _) = d

instance (Eq a, Ord a) => Group (Permutation a) where
    e = Perm [] [[]]
    inverse (Perm ns cs) = Perm ns $ reverse $ map reverse cs
    (Perm ns cs) .+ (Perm ms ds) = Perm (sort $ nub (ns ++ ms)) (cs ++ ds)

instance (Eq a, Ord a) => Eq (Permutation a) where
    o == p = (map (pApply o) dom) == (map (pApply p) dom)
        where
          dom = pDomain (o .+ p)

instance Show a => Show (Permutation a) where
    show (Perm [] _) = "id"
    show (Perm _ cs) = concatMap showCycle cs
        where
          showCycle c = "("  ++ (intercalate " " (map show c)) ++ ")"


pApply :: Eq a => Permutation a -> a -> a
pApply (Perm d cs) = foldr1 (.) (reverse $ map applyCycle cs)

applyCycle :: Eq a => Cycle a -> a -> a
applyCycle c x = case elemIndex x c of
                   (Just i) -> (cycle c) !! (i+1)
                   Nothing -> x

swapTop = Perm [1,2] [[1,2]]
swapBottom = Perm [3,4] [[3,4]]

simplifyPermutation :: (Eq a, Ord a) => Permutation a -> Permutation a
simplifyPermutation (Perm _ []) = Perm [] []
simplifyPermutation p@(Perm [] _) = Perm [] []
simplifyPermutation p@(Perm (x:xs) cs) = q .+ q'
    where
      cs' = [(simplifyFrom x x p)]
      q = Perm (nub $ concat cs') cs'
      q' = simplifyPermutation $ Perm ((x:xs) \\ pDomain q) cs

--(Perm (x:xs) (simplifyFrom g x p)) .+ (Perm 

simplifyFrom :: Eq a =>  a -> a -> Permutation a -> [a]
simplifyFrom g x p | y == g = [x]
                   | otherwise = x : simplifyFrom g y p
    where
      y = pApply p x


test = Perm [1,2,3,4,5] [[1,2],[1,3],[4,5]]