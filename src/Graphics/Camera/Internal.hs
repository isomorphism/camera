{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Camera.Internal where

import Data.Typeable

import Control.Lens
import Linear

import Graphics.Camera.Angle
import Graphics.Camera.Classes

-- | Generic 3D camera
data BaseCamera a = BCam
    { _bcamViewport    :: V2 a
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


instance Camera OrthoCam where
    viewArea = ocamBaseCamera.bcamViewport


instance Camera3D OrthoCam where
    viewMatrix c = mkTransformation (c^.orientation) (c^.position)
    invViewMatrix c = mkTransformation (c^.orientation & _ijk.each %~ negate) (c^.position & each %~ negate)
    projMatrix c = ortho (c^.leftClip)   (c^.rightClip) 
                         (c^.bottomClip) (c^.topClip)
                         (c^.nearLimit)  (c^.farLimit)
    invProjMatrix c = inverseOrtho (c^.leftClip)   (c^.rightClip) 
                                   (c^.bottomClip) (c^.topClip)
                                   (c^.nearLimit)  (c^.farLimit)
    rangeLimit = ocamBaseCamera.bcamViewRange
    position = ocamBaseCamera.bcamPosition
    orientation = ocamBaseCamera.bcamOrientation

instance OrthographicCamera OrthoCam where
    horizontalRange = ocamHRange
    verticalRange = ocamVRange

instance Camera Cam where
    viewArea = pcamBaseCamera.bcamViewport

instance Camera3D Cam where
    viewMatrix c = mkTransformation (c^.orientation) (c^.position)
    invViewMatrix c = mkTransformation (c^.orientation & _ijk.each %~ negate) (c^.position & each %~ negate)
    projMatrix c = perspective (c^.fovVertical.radians) (c^.fovAspect) (c^.nearLimit) (c^.farLimit)
    invProjMatrix c = inversePerspective (c^.fovVertical.radians) (c^.fovAspect) (c^.nearLimit) (c^.farLimit)
    rangeLimit = pcamBaseCamera.bcamViewRange
    position = pcamBaseCamera.bcamPosition
    orientation = pcamBaseCamera.bcamOrientation

instance PerspectiveCamera Cam where
    focalArea = pcamFocalArea
    focalLength = pcamFocalLength



