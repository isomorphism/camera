{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Camera.Angle where

import Foreign.Storable

import Control.Lens



-- | Wrapper type representing an angle.
newtype Angle a = Angle a
  deriving (Eq, Ord, Read, Show, Storable, Num, Fractional)

-- | Construct an angle using an angle unit isomorphism, e.g.:
--
--   @'angleFrom' 'degrees' 60@ ≡ @'angleFrom' 'turns' (1/6)@ ≡ @'angleFrom' 'radians' ('pi'/3)@
angleFrom :: AnIso' (Angle a) a -> a -> Angle a
angleFrom l = view . from $ l

-- | Converts between abstract 'Angle's and "turns", or fractions of a complete circle.
turns :: Iso' (Angle a) a
turns = iso (\(Angle a) -> a) Angle

-- | Converts between abstract 'Angle's and radians.
radians :: (Floating a) => Iso' (Angle a) a
radians = turns . iso (* τ) (/ τ)

-- | Converts between abstract 'Angle's and degrees. 
degrees :: (Fractional a) => Iso' (Angle a) a
degrees = turns . iso (* 360) (/ 360)

-- | The circle constant, relating turns to radians.
τ :: (Floating a) => a
τ = 2 * pi
