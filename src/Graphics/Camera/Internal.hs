{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Camera.Internal where

import Data.Typeable

import Control.Lens
import Linear

import Graphics.Camera.Angle
import Graphics.Camera.Classes


makeLenses ''BaseCamera

-- | Orthographic projection camera
newtype OrthoCam a = OCam { _ocamBaseCamera :: BaseCamera a }
  deriving (Eq, Ord, Typeable)

makeLenses ''OrthoCam

-- | Perspective projection camera
data Cam a = PCam
    { _pcamBaseCamera  :: BaseCamera a
    , _pcamFocalArea :: Maybe (V2 a)
    , _pcamFocalLength :: a
    } deriving (Eq, Ord, Typeable)

makeLenses ''Cam

class CameraLike c where
    baseCamera :: Lens' (c a) (BaseCamera a)
instance CameraLike OrthoCam where
    baseCamera = ocamBaseCamera
instance CameraLike Cam where
    baseCamera = pcamBaseCamera



instance Camera BaseCamera where
    viewArea = bcamViewport
    rangeLimit = bcamViewRange
    position = bcamPosition
    orientation = bcamOrientation

instance Camera OrthoCam where
    cameraMatrix = cameraMatrix . view baseCamera
    invCameraMatrix = invCameraMatrix . view baseCamera
    viewMatrix = viewMatrix . view baseCamera
    invViewMatrix = invViewMatrix . view baseCamera
    projMatrix = projMatrix . view baseCamera
    invProjMatrix = invProjMatrix . view baseCamera
    viewArea = baseCamera.viewArea
    rangeLimit = baseCamera.rangeLimit
    position = baseCamera.position
    orientation = baseCamera.orientation
    

instance Camera Cam where
    viewArea = baseCamera.viewArea
    rangeLimit = baseCamera.rangeLimit
    position = baseCamera.position
    orientation = baseCamera.orientation

instance PerspectiveCamera Cam where
    focalArea = pcamFocalArea
    focalLength = pcamFocalLength


-- | Compute the focal length for a given sensor size and FOV angle.
fromFOV :: (Floating a) => a -> Angle a -> a
fromFOV d a = d / (2 * tan (a^.radians / 2))

-- | Compute the field of view for a given sensor size and focal length.
toFOV :: (Floating a) => a -> a -> Angle a
toFOV d f = angleFrom radians $ 2 * atan (d / (2 * f))

-- | For a constant sensor size we have an isomorphism between FOV and focal length
isoFOV :: (Floating a) => a -> Iso' a (Angle a)
isoFOV x = iso (toFOV x) (fromFOV x) -- . from radians


