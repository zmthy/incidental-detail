{-# LANGUAGE GeneralizedNewtypeDeriving #-}

------------------------------------------------------------------------------
module Graphics.DetailGen.Monad
    (
    -- * Shape
      Shape (..)

    -- * Detail
    , Detail (..)

    -- * DetailGen
    , DetailGen
    , runDetailGen
    , detail
    , branch
    , pureBranch
    , done
    ) where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.Free
import Data.Tree

------------------------------------------------------------------------------
import Graphics.DetailGen.PointSelection
import Graphics.DetailGen.Vec3


------------------------------------------------------------------------------
data Shape = Cube | Cylinder | Sphere
    deriving (Show)


------------------------------------------------------------------------------
data Detail = Detail
    { detailShape     :: Shape
    , detailSelection :: PointSelection
    , detailGScale    :: Vec3
    , detailPScale    :: Vec3
    , detailUp        :: Vec3
    } deriving (Show)


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
detail :: Shape -> PointSelection -> Vec3 -> Vec3 -> Vec3 -> DetailGen ()
detail s sp gs ps u = mk $ Apply (Detail s sp gs ps u) [pure ()]


------------------------------------------------------------------------------
branch :: [DetailGen a] -> DetailGen a
branch = mk . Branch . map runDetailGen'


------------------------------------------------------------------------------
pureBranch :: [a] -> DetailGen a
pureBranch = branch . map (DetailGen . Pure)


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

