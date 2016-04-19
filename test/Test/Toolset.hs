module Test.Toolset where
import System.Random


-- |
-- generates values distributed as the Gumbel distribution
-- parameters: Random Generator (Uniform unit distribution), mu, beta
genGumbel :: (Floating a, Random a, RandomGen b) => b -> a -> a -> [a]
genGumbel g mu beta = map (\x -> mu - beta * log(-log(x))) $ randoms g

-- |
-- generates values distributed as the Weibull distribution
-- parameters: Random Generator (Uniform unit distribution), lambda, k
genWeibull :: (Floating a, Random a, RandomGen b) => b -> a -> a -> [a]
genWeibull g lambda k = map (\x -> lambda * (-log(1-x)) ** (1/k) ) $ randoms g

-- |
-- generates values distributed as the Uniform distribution
-- parameters: Random Generator (Uniform unit distribution), a, b (limits)
genUniform :: (Floating a, Random a, RandomGen b) => b -> a -> a -> [a]
genUniform g a b = map (\x -> x*(b-a) + a) $ randoms g

-- g <- newStdGen --getStdGen



