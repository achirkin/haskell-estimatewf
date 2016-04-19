{-# LANGUAGE FlexibleContexts #-}
module Workflow.Toolset
    ( Array1D
    , mean, var, characteristicF, distributionF
    , fft, ifft, distToChar, charToDist, linSpace, asort, azipWith
    ) where

import Data.Array.Unboxed
import Data.Complex
import Data.Bits as Bit
import Data.Array.ST
import Control.Monad.ST
import Data.Array.Base

-- | Array indexed by integers
type Array1D = UArray Int

-- | mean estimation.
-- Complexity O(n).
mean :: (IArray UArray a, Fractional a) => Array1D a -> a
mean arr = s / size where
    (i1,i2) = bounds arr
    size = fromIntegral (i2 - i1 + 1)
    s = afoldl (+) 0 arr

-- | Unbiased variance estimation.
-- Complexity O(n).
var :: (IArray UArray a, Fractional a) => Array1D a -> a
var arr = s / size where
    (i1,i2) = bounds arr
    size = fromIntegral (i2 - i1)
    me = mean arr
    s = afoldl (\acc x -> acc + (x - me)^(2::Int)) 0 arr

-- |
-- Empirical characteristic function of the distribuition represented by array (sample).
-- Complexity O(n).
characteristicF :: (IArray UArray a, RealFloat a) => Array1D a -> [a] -> [Complex a]
characteristicF _ [] = []
characteristicF arr ts = size `seq` ts `seq` map ((/size) . f) ts
  where
    (i1,i2) = bounds arr
    size = fromIntegral (i2 - i1 + 1)
    f t = t `seq` afoldl g 0 arr where
        g acc x = x `seq` acc `seq` acc + (cis $ x*t)


-- |
-- Empirical cumulative distribuition function estimation
-- of the distribuition represented by !sorted! array (sample).
-- Uses linear approximation between points.
-- Complexity O(n).
distributionF :: (IArray UArray a, RealFloat a) => Array1D a -> [a] -> [a]
distributionF _ [] = []
distributionF arr tt@(t:ts) = if t <= (arr ! i1)
        then 0:(distributionF arr ts)
        else go (i1+1) tt
  where
    (i1,i2) = bounds arr
    n = fromIntegral (i2 - i1 + 1)
    go _ [] = []
    go i xt@(x:xs) | i == i2     = take (length xt) . repeat $ 1
                   | arr ! i > x = ( (fromIntegral (i - i1) +
                     (x - (arr ! (i-1))) / ((arr ! i) - (arr ! (i-1)))
                     )/ n):(go i xs)
                   | otherwise   = go (i+1) tt

-- | Converts CDF into Char Func., given grid (x,t)
-- Note, length of t is n/2, and length of x is n
-- resulting array also has length of n/2, but CDF array has length of n
distToChar :: ([Double],[Double]) -> [Double] -> [Complex Double]
distToChar (x,t) f = zipWith (\t' f' -> f' * (0 :+ t'*c) * (cis $ t'*a)) t phi'
  where
    (a,b) = (head x, last x)
    c = (a-b)/(n-1)
    n = fromIntegral . flip Bit.shiftL 1 . length $ t  :: Double
    phi' = reverse . fft True $ zipWith (\f' x' -> (f' - (x'-a)/(b-a)) :+ 0) f x

-- | Converts Char Func. into CDF, given grid (x,t)
-- Note, length of t is n/2, and length of x is n
-- resulting array also has length of n, but Char Func. array has length of n/2
charToDist :: ([Double],[Double]) -> [Complex Double] -> [Double]
charToDist (x,t) phi = postPass
    . shiftUp
    $ zipWith (\x' ps -> (realPart ps) + (x'-a)/(b-a)) x $ ifft False psi
  where
    (a,b) = (head x, last x)
    c = (n-1)/(a-b)
    n' = length $ t  :: Int
    n = fromIntegral . flip Bit.shiftL 1 $ n' :: Double
    phi' = zipWith (\t' ph -> conjugate ph * (0 :+ c/t') * (cis $ t'*a)) t phi
    psi = (:) 0 $ (++) phi' . tail . reverse . map conjugate $ phi'
    shiftUp xxs'@(x':_) = map (\z -> z-x') xxs'
    shiftUp [] = []
    postPass xs = map (min 1 . max 0) xs


-- | Fast Fourier Transformation.
-- Only accepts sizes n = 2^k.
-- Boolean parameter is whether to return full array, or only second (right) part of it.
fft :: RealFloat a => Bool -> [Complex a] -> [Complex a]
fft halve xs = fft' xs w halve
  where
    n = length $ xs
    w = cis $ -2 * pi / fromIntegral n

-- | Inverse Fast Fourier Transformation.
-- Only accepts sizes n = 2^k.
-- Boolean parameter is whether to return full array, or only second (right) part of it.
ifft :: RealFloat a => Bool -> [Complex a] -> [Complex a]
ifft halve xs = map (/ fromIntegral n) $ fft' xs w halve
  where
    n = length $ xs
    w = cis $ 2 * pi / fromIntegral n

-- Fourier transform (both, forward and inverse)
fft' :: Num t => [t] -> t -> Bool -> [t]
fft' []  _ _ = []
fft' [x] _ _ = [x]
fft' xs  w halve = xs `seq` ys `seq` zs `seq` w `seq` halve `seq`
    if halve then zipWith (-) ys ts
             else zipWith (+) ys ts ++ zipWith (-) ys ts
    where wx  = w*w
          ys = fft' evens wx False
          zs = fft' odds wx False
          (evens, odds)  = split xs
          split []       = ([], [])
          split [x]       = ([x], [])
          split (x:y:ss) = (x:xt, y:yt) where (xt, yt) = split ss
          ts = zipWith (*) zs . iterate (w*) $ 1

-- |
-- create x and t grids from bounds on x;
-- Arguments: k power of 2 - number of points: n = 2^k; bounds
linSpace :: Int -> (Double,Double) -> ([Double],[Double])
linSpace k (a,b) = (map (\i' -> a + i' * (b-a) / (n-1)) i, map (\j' -> 2*pi*j'*(n-1)/n/(b-a)) j)
  where
    n = fromIntegral $ (Bit.shiftL 1 k :: Int ):: Double
    i = [0..n-1] :: [Double]
    j = [1..n/2] :: [Double]



-- import Data.Foldable
-- | Left fold on an array
afoldl :: (IArray UArray a) => (b -> a -> b) -> b -> Array1D a -> b
afoldl f x0 arr = f `seq` x0 `seq` arr `seq` go x0 start where
    (start, end) = bounds arr
    g x y = x `seq` y `seq` f x y
    go x i | i > end   = x
           | otherwise = x `seq` go (g x (arr ! i)) (i+1)


---- | Right fold on an array
--afoldr :: (IArray UArray a) => (a -> b -> b) -> b -> Array1D a -> b
--afoldr f x0 arr = f `seq` x0 `seq` arr `seq` go x0 end where
--    (start, end) = bounds arr
--    go x i | i < start = x
--           | otherwise = x `seq` go (f (arr ! i) x) (i-1)


-- | zipWith for arrays
azipWith :: (IArray UArray a, IArray UArray b, IArray UArray c) => (a -> b -> c) -> Array1D a -> Array1D b -> Array1D c
azipWith f a b = a `seq` b `seq` listArray bnds [f (a ! i) (b ! i) | i <- range bnds] where
    bnds = bounds a



-- | working implementation of mergesort; uses the same array of additional memory, slow just as original Data.List.sort
asort :: Array1D Double -> Array1D Double
asort arr = if size < 2 then arr
                        else runSTUArray result
  where
  (start,end) = bounds arr :: (Int, Int)
  size = end-start+1 :: Int
  result = do
    xRight <- (thaw :: Array1D Double -> ST s (STUArray s Int Double)) arr
    xLeft <- (thaw :: Array1D Double -> ST s (STUArray s Int Double)) arr
    let f _ 1 _ = return ()
        f i 2 right = let x = if right then xLeft else xRight
                      in do
          x1 <- unsafeRead x i
          x2 <- unsafeRead x (i+1)
          if x1 > x2 then do
            unsafeWrite x i x2
            unsafeWrite x (i+1) x1
          else return ()
        f i 3 right = let x = if right then xLeft else xRight in do
          x1 <- unsafeRead x i
          x2 <- unsafeRead x (i+1)
          x3 <- unsafeRead x (i+2)
          case (x1 > x2, x1 > x3, x2 > x3) of
            (True,False,False) -> do
                unsafeWrite x i x2
                unsafeWrite x (i+1) x1
            (True,True,True) -> do
                unsafeWrite x i x3
                unsafeWrite x (i+2) x1
            (False,False,True) -> do
                unsafeWrite x (i+1) x3
                unsafeWrite x (i+2) x2
            (False,True,True) -> do
                unsafeWrite x i x3
                unsafeWrite x (i+1) x1
                unsafeWrite x (i+2) x2
            (True,True,False) -> do
                unsafeWrite x i x2
                unsafeWrite x (i+1) x3
                unsafeWrite x (i+2) x1
            _ -> return ()
        f i n right = do
            f i n1 (not right)
            f (i+n1) n2 (not right)
            merge 0 0
          where
            x = if right then xRight else xLeft
            y = if right then xLeft else xRight
            n1 = shiftR n 1
            n2 = n - n1
            i2 = i+n1
            merge k j | k == n1 && j < n2  = sequence_
                          [unsafeRead x (i2+m) >>= unsafeWrite y (i+k+m) | m <- range (j,n2-1)]
                      | k < n1 && j == n2  = sequence_
                          [unsafeRead x (i +m) >>= unsafeWrite y (i+m+j) | m <- range (k,n1-1)]
                      | k < n1 && j < n2 = do
               x1 <- unsafeRead x (i+k)
               x2 <- unsafeRead x (i2+j)
               if x1 > x2 then unsafeWrite y (i+k+j) x2 >> merge k (j+1)
                          else unsafeWrite y (i+k+j) x1 >> merge (k+1) j
                      | otherwise = return ()
    f 0 size True
    return xLeft

