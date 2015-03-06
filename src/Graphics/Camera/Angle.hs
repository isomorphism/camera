{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Camera.Angle where

import Foreign.Storable

import Control.Lens
import Numeric.Lens

import Linear

newtype Angle a = 
    Angle a
  deriving (Eq, Ord, Read, Show, Storable, Num, Fractional)


angleFrom :: AnIso' (Angle a) a -> a -> Angle a
angleFrom l = view . from $ l

turns :: Iso' (Angle a) a
turns = iso (\(Angle a) -> a) Angle

radians :: (Floating a) => Iso' (Angle a) a
radians = turns . iso (* τ) (/ τ)

degrees :: (Fractional a) => Iso' (Angle a) a
degrees = turns . iso (* 360) (/ 360)

τ :: (Floating a) => a
τ = 2 * pi
