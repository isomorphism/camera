{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Graphics.Camera.Internal 
    ( module Graphics.Camera.Internal
    , module Graphics.Camera.Types
    ) where

import Control.Applicative
import Data.Foldable
import Data.Maybe
import Data.Typeable

import Control.Lens
import Linear

import Graphics.Camera.Angle
import Graphics.Camera.Types


axisVector :: (Foldable v, Additive v, Additive t, Num a) => v a -> Lens' (v (t a)) (t a)
axisVector fwd = lens get set
  where fwd' = V1 <$> fwd
        get m = fwd *! m 
        set m v = m !+! fwd' !*! (V1 v !-! V1 fwd !*! m)

-- | Compute the focal length for a given sensor size and FOV angle.
toFocalLength :: (Floating a) => a -> Angle a -> a
toFocalLength d a = d / (2 * tan (a^.radians / 2))

-- | Compute the sensor size for a focal length and FOV angle.
toSensorSize :: (Floating a) => a -> Angle a -> a
toSensorSize l a = l * (2 * tan (a^.radians / 2))

-- | Compute the field of view for a given sensor size and focal length.
toFOV :: (Floating a) => a -> a -> Angle a
toFOV d f = angleFrom radians $ 2 * atan (d / (2 * f))

-- | For a constant sensor size we have an isomorphism between FOV and focal length
isoFocalLengthFOV :: (Floating a) => a -> Iso' a (Angle a)
isoFocalLengthFOV sz = iso (toFOV sz) (toFocalLength sz)

-- | For a constant focal length we have an isomorphism between FOV and sensor size
isoSensorFOV :: (Floating a) => a -> Iso' a (Angle a)
isoSensorFOV fl = iso (flip toFOV fl) (toSensorSize fl)



