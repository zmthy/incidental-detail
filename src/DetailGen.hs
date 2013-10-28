{-# LANGUAGE GeneralizedNewtypeDeriving #-}

------------------------------------------------------------------------------
module DetailGen
    (
    -- * Shape
      Shape (..)

    , Detail (..)

    -- * DetailGen
    , DetailGen
    , runDetailGen
    , detail
    , branch
    , done
    ) where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.Free
import Data.Tree

------------------------------------------------------------------------------
import PointSelect


------------------------------------------------------------------------------
data Shape = Cube | Cylinder
    deriving (Show)


------------------------------------------------------------------------------
data Detail = Detail Shape PointSelect Double
    deriving (Show)


------------------------------------------------------------------------------
data DetailBranch g
    = Apply Detail [g]
    | Branch [g]
    deriving (Show)

instance Functor DetailBranch where
    fmap f (Apply d gs) = Apply d $ map f gs
    fmap f (Branch gs)  = Branch $ map f gs


------------------------------------------------------------------------------
newtype DetailGen a = DetailGen { runDetailGen' :: Free DetailBranch a }
    deriving (Functor, Applicative, Monad)

mk :: DetailBranch (Free DetailBranch a) -> DetailGen a
mk = DetailGen . Free


------------------------------------------------------------------------------
detail :: Shape -> PointSelect -> Double -> DetailGen ()
detail shape select scale = mk $ Apply (Detail shape select scale) [pure ()]


------------------------------------------------------------------------------
branch :: [DetailGen a] -> DetailGen a
branch = mk . Branch . map runDetailGen'


------------------------------------------------------------------------------
done :: DetailGen ()
done = mk $ Branch []


------------------------------------------------------------------------------
runDetailGen :: DetailGen () -> Forest Detail
runDetailGen = unFree . runDetailGen'
  where
    unFree (Pure _) = []
    unFree (Free b) = unBranch b
    unBranch (Apply d fs) = [Node d (concatMap unFree fs)]
    unBranch (Branch fs)  = concatMap unFree fs

