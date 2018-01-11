{-# OPTIONS_GHC -Wall #-}

module HW11.ApplicativeUtil where
import Prelude hiding (sequenceA)

(*>) :: Applicative f => f a -> f b -> f b
_ *> y = y

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = sequenceA . map f

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (\x -> (<*>) ((:) <$> x)) (pure [])


replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n x = sequenceA $ replicate n x
