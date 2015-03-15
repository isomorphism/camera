{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Camera.Types where

import Control.Applicative
import Data.Data
import Data.Maybe
import Data.Typeable

import Control.Lens
import Linear

import Graphics.Camera.Angle

-- | Generic 3D camera
data BaseCamera a = BCam
    { _bcamCoords      :: M33 a
    , _bcamViewport    :: V2 a
    , _bcamViewRange   :: (a, a)
    , _bcamPosition    :: V3 a
    , _bcamOrientation :: Quaternion a
    } deriving (Eq, Ord, Typeable)

makeLenses ''BaseCamera

-- | Orthographic projection camera
data OrthoCam a = OCam 
    { _ocamBaseCamera :: BaseCamera a 
    , _ocamHRange :: (a, a)
    , _ocamVRange :: (a, a)
    }
  deriving (Eq, Ord, Typeable)

makeLenses ''OrthoCam

-- | Perspective projection camera
data Cam a = PCam
    { _pcamBaseCamera  :: BaseCamera a
    , _pcamFocalArea :: Maybe (V2 a)
    , _pcamFocalLength :: a
    } deriving (Eq, Ord, Typeable)

makeLenses ''Cam

-- | Modifier that applies an offset to a camera in its local coordinate space.
data Jib c a = Jib
    { _jibCamera       :: c a
    , _jibDisplacement :: !(V3 a)
    } deriving (Eq, Ord, Typeable)

makeLenses ''Jib

newJib :: (Num a) => c a -> Jib c a
newJib c = Jib c 0


-- | Modifier that applies a sequence of rotations to a camera in its local 
--   coordinate space.
data Gimbal c a = Gimbal
    { _gimbalCamera    :: c a
    , _gimbalHeading   :: !(Angle a)
    , _gimbalElevation :: !(Angle a)
    , _gimbalRoll      :: !(Angle a)
    } deriving (Eq, Ord, Typeable)

makeLenses ''Gimbal

newGimbal :: (Num a) => c a -> Gimbal c a
newGimbal c = Gimbal c 0 0 0

-- | Describes the coordinate system of an unrotated camera.
data CoordinateSystem a
        -- | Coordinate system common in 3D modeling applications. The positive Z
        --   axis points up and the positive Y axis points forward.
        = VerticalZ 
        -- | Coordinate system common in 3D graphics programming. The positive Y
        --   axis points up and the positive Z axis points forward. This is a 
        --   left-handed coordinate system.
        | ZForward
        -- | Coordinate system common in 3D graphics programming. The positive Y
        --   axis points up and the positive Z axis points backward. This is a 
        --   right-handed coordinate system.
        | ZBackward
        -- | Arbitrary coordinate system, specified as @('V3' rightward upward forward)@
        | OtherCoords (M33 a)
  deriving (Eq, Ord, Read, Show, Data, Typeable)


