module Groups
where

class Group g where
    e :: g
    inverse :: g -> g
    (.+) :: g -> g -> g

    (.-) :: g -> g -> g
    x .- y = x .+ (inverse y)

    lexp, rexp :: g -> Int -> g -- 
    lexp x n = foldl1 (.+) (replicate n x)
    rexp x n = foldr1 (.+) (replicate n x)

generateCyclicSubgroup :: (Group g, Eq g) => g -> [g]
generateCyclicSubgroup g = generateFrom e g
    where
      generateFrom x g | y == e = [x]
                       | otherwise = x : generateFrom y g
          where
            y = x .+ g

    
