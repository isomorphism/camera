{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Camera.Internal where

import Control.Applicative
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
    orthoCamera v@(V2 w h) near far = OCam (BCam eye3 v (near, far) 0 1) (0, w) (0, h)
    orthoFromPlanes v l r b t n f = OCam (BCam eye3 v (n, f) 0 1) (l, r) (b, t)
    horizontalRange = ocamHRange
    verticalRange = ocamVRange

instance Camera Cam where
    viewArea = pcamBaseCamera.bcamViewport

instance Camera3D Cam where
    coordinateSystem = pcamBaseCamera.bcamCoords.iso (_z %~ negate) (_z %~ negate)
    viewMatrix = to $ \c -> mkTransformation (c^.orientation) (c^.position)
    invViewMatrix = to $ \c -> mkTransformation (c^.orientation & _ijk.each %~ negate) (c^.position & each %~ negate)
    projMatrix = to $ 
        \c -> perspective (c^.fovVertical.radians) (c^.viewAspect) (c^.nearLimit) (c^.farLimit)
          !*! mkTransformationMat (c^.coordinateSystem) 0
    invProjMatrix = to $ 
        \c -> mkTransformationMat (fromMaybe (error "non-orthogonal coordinate system") (inv33 $ c^.coordinateSystem)) 0
          !*! inversePerspective (c^.fovVertical.radians) (c^.viewAspect) (c^.nearLimit) (c^.farLimit) 
    rangeLimit = pcamBaseCamera.bcamViewRange
    position = pcamBaseCamera.bcamPosition
    orientation = pcamBaseCamera.bcamOrientation

instance PerspectiveCamera Cam where
    perspectiveCamera v cd fl near far = PCam (BCam coordsMatrix v (near, far) 0 1) Nothing fl
      where coordsMatrix = case cd of
                VerticalZ -> V3 (V3 1 0 0) (V3 0 0 1) (V3 0 1 0)
                ZForward  -> V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
                ZBackward -> V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 (-1))
                OtherCoords m -> m
    focalArea = pcamFocalArea
    focalLength = pcamFocalLength


data Jib c a = Jib
    { _jibCamera       :: c a
    , _jibDisplacement :: !(V3 a)
    } deriving (Eq, Ord, Typeable)

makeLenses ''Jib

newJib :: (Num a) => c a -> Jib c a
newJib c = Jib c 0

instance (Camera3D c) => JibCamera (Jib c) where
    displacement = jibDisplacement

instance (GimbalCamera c) => GimbalCamera (Jib c) where
    heading = jibCamera.heading
    elevation = jibCamera.elevation
    roll = jibCamera.roll


instance (Camera c) => Camera (Jib c) where
    viewArea = jibCamera.viewArea

instance (Camera3D c) => Camera3D (Jib c) where
    coordinateSystem = jibCamera.coordinateSystem
    viewMatrix = to $ \c -> mkTransformationMat eye3 (negate <$> c^.displacement) !*! c^.jibCamera.viewMatrix 
    invViewMatrix = to $ \c -> c^.jibCamera.invViewMatrix !*! mkTransformationMat eye3 (c^.displacement)
    projMatrix = jibCamera.projMatrix
    invProjMatrix = jibCamera.invProjMatrix
    position = jibCamera.position
    orientation = jibCamera.orientation
    rangeLimit = jibCamera.rangeLimit

instance (PerspectiveCamera c) => PerspectiveCamera (Jib c) where
    perspectiveCamera v cd fl near far = newJib $ perspectiveCamera v cd fl near far
    focalArea = jibCamera.focalArea
    focalLength = jibCamera.focalLength

instance (OrthographicCamera c) => OrthographicCamera (Jib c) where
    orthoCamera v near far = newJib $ orthoCamera v near far 
    orthoFromPlanes v l r b t n f = newJib $ orthoFromPlanes v l r b t n f
    horizontalRange = jibCamera.horizontalRange
    verticalRange = jibCamera.verticalRange


data Gimbal c a = Gimbal
    { _gimbalCamera    :: c a
    , _gimbalHeading   :: !(Angle a)
    , _gimbalElevation :: !(Angle a)
    , _gimbalRoll      :: !(Angle a)
    } deriving (Eq, Ord, Typeable)

makeLenses ''Gimbal

newGimbal :: (Num a) => c a -> Gimbal c a
newGimbal c = Gimbal c 0 0 0

instance (Camera3D c) => GimbalCamera (Gimbal c) where
    heading = gimbalHeading
    elevation = gimbalElevation
    roll = gimbalRoll

instance (JibCamera c) => JibCamera (Gimbal c) where
    displacement = gimbalCamera.displacement

instance (Camera c) => Camera (Gimbal c) where
    viewArea = gimbalCamera.viewArea

instance (Camera3D c) => Camera3D (Gimbal c) where
    coordinateSystem = gimbalCamera.coordinateSystem
    viewMatrix = to $ 
        \c -> let rot = m33_to_m44 . fromQuaternion $ c^.gimbalRotation
              in rot !*! c^.gimbalCamera.viewMatrix 
    invViewMatrix = to $ 
        \c -> let rot = m33_to_m44 . fromQuaternion $ c^.invGimbalRotation
              in c^.gimbalCamera.invViewMatrix !*! rot
    projMatrix = gimbalCamera.projMatrix
    invProjMatrix = gimbalCamera.invProjMatrix
    position = gimbalCamera.position
    orientation = gimbalCamera.orientation
    rangeLimit = gimbalCamera.rangeLimit

instance (PerspectiveCamera c) => PerspectiveCamera (Gimbal c) where
    perspectiveCamera v cd fl near far = newGimbal $ perspectiveCamera v cd fl near far
    focalArea = gimbalCamera.focalArea
    focalLength = gimbalCamera.focalLength

instance (OrthographicCamera c) => OrthographicCamera (Gimbal c) where
    orthoCamera v near far = newGimbal $ orthoCamera v near far 
    orthoFromPlanes v l r b t n f = newGimbal $ orthoFromPlanes v l r b t n f
    horizontalRange = gimbalCamera.horizontalRange
    verticalRange = gimbalCamera.verticalRange


