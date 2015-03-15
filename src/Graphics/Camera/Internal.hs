{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Camera.Internal where

import Data.Maybe
import Data.Typeable

import Control.Lens
import Linear

import Graphics.Camera.Angle
import Graphics.Camera.Classes

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


instance Camera OrthoCam where
    viewArea = ocamBaseCamera.bcamViewport


instance Camera3D OrthoCam where
    coordinateSystem = ocamBaseCamera.bcamCoords.iso (_z %~ negate) (_z %~ negate)
    viewMatrix = to $ \c -> mkTransformation (c^.orientation) (c^.position)
    invViewMatrix = to $ \c -> mkTransformation (c^.orientation & _ijk.each %~ negate) (c^.position & each %~ negate)
    projMatrix = to $ 
        \c -> ortho (c^.leftClip)   (c^.rightClip) 
                    (c^.bottomClip) (c^.topClip)
                    (c^.nearLimit)  (c^.farLimit)
          !*! mkTransformationMat (c^.coordinateSystem) 0
    invProjMatrix = to $ 
        \c -> mkTransformationMat (fromMaybe (error "non-orthogonal coordinate system") (inv33 $ c^.coordinateSystem)) 0
          !*! inverseOrtho (c^.leftClip)   (c^.rightClip) 
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
    coordinateSystem = pcamBaseCamera.bcamCoords.iso (_z %~ negate) (_z %~ negate)
    viewMatrix = to $ \c -> mkTransformation (c^.orientation) (c^.position)
    invViewMatrix = to $ \c -> mkTransformation (c^.orientation & _ijk.each %~ negate) (c^.position & each %~ negate)
    projMatrix = to $ 
        \c -> perspective (c^.fovVertical.radians) (c^.fovAspect) (c^.nearLimit) (c^.farLimit)
          !*! mkTransformationMat (c^.coordinateSystem) 0
    invProjMatrix = to $ 
        \c -> mkTransformationMat (fromMaybe (error "non-orthogonal coordinate system") (inv33 $ c^.coordinateSystem)) 0
          !*! inversePerspective (c^.fovVertical.radians) (c^.fovAspect) (c^.nearLimit) (c^.farLimit) 
    rangeLimit = pcamBaseCamera.bcamViewRange
    position = pcamBaseCamera.bcamPosition
    orientation = pcamBaseCamera.bcamOrientation

instance PerspectiveCamera Cam where
    focalArea = pcamFocalArea
    focalLength = pcamFocalLength



