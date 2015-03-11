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
    , _pcamFocalLength :: a
    , _pcamFocus :: a
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
    -- TODO



