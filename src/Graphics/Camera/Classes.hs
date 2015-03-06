{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Camera.Classes where

import GHC.Exts
import Control.Lens
import Linear
import Graphics.Camera.Angle

class Camera c where
    type CamView c :: * -> Constraint
    
    cameraMatrix    :: (CamView c a) => c a -> M44 a
    invCameraMatrix :: (CamView c a) => c a -> M44 a
    viewMatrix      :: (CamView c a) => c a -> M44 a
    invViewMatrix   :: (CamView c a) => c a -> M44 a
    projMatrix      :: (CamView c a) => c a -> M44 a
    invProjMatrix   :: (CamView c a) => c a -> M44 a
    viewArea        :: Lens' (c a) (V2 a)
    viewWidth       :: Lens' (c a) a
    viewHeight      :: Lens' (c a) a
    viewAspect      :: (Floating a) => Lens' (c a) a
    viewDiagonal    :: (Floating a) => Lens' (c a) a
    viewRange       :: Lens' (c a) (a, a)
    nearLimit       :: Lens' (c a) a
    farLimit        :: Lens' (c a) a
    position        :: Lens' (c a) (V3 a)
    orientation     :: Lens' (c a) (Quaternion a)
    
    viewWidth    = viewArea._x
    viewHeight   = viewArea._y
    viewAspect   = lens (\c -> let V2 x y = c^.viewArea in x / y)
                        (\c a -> let d = c^.viewDiagonal
                                     y = d / sqrt (a^2 + 1) 
                                 in c & viewArea .~ V2 (a * y) y)
    viewDiagonal = lens (norm . view viewArea) 
                        (\c d -> let a = c^.viewAspect
                                     y = d / sqrt (a^2 + 1) 
                                 in c & viewArea .~ V2 (a * y) y)
    nearLimit    = viewRange._1
    farLimit     = viewRange._2


class (Camera c) => ProjCamera c where
    fieldOfView   :: (Floating a) => c a -> V2 (Angle a)
    fovHorizontal :: (Floating a) => Lens' (c a) (Angle a)
    fovVertical   :: (Floating a) => Lens' (c a) (Angle a)
    focalLength   :: (Floating a) => Lens' (c a) a
    focus         :: (Floating a) => Lens' (c a) a

    fieldOfView c   = V2 (c^.fovHorizontal) (c^.fovVertical)
    fovHorizontal = focalLength . mkIsoFOV 36 -- TODO
    fovVertical = focalLength . mkIsoFOV 24 -- TODO
    

class (Camera c) => MobileCamera c where
    moveCameraTo :: (Floating a) => c a -> c a -> a -> c a

class (Camera (t c), Camera c) => TargettedCamera t c where
    target         :: V3 a -> c a -> t c a
    untarget       :: t c a -> c a
    targetPosition :: Lens' (t c a) (V3 a)
    targetDistance :: (Floating a) => Lens' (t c a) a
    upVector       :: Lens' (t c a) (V3 a)
    forwardVector  :: Lens' (t c a) (V3 a)
    azimuth        :: Lens' (t c a) (Angle a)
    altitude       :: Lens' (t c a) (Angle a)
    roll           :: Lens' (t c a) (Angle a)
    
    targetDistance = lens (\c -> distance (c^.targetPosition) (c^.position))
                          (\c d -> let off = fmap (d *) (c^.targetPosition - c^.position)
                                   in c & position .~ off + c^.targetPosition)
    

toFOV x f = x / (2 * tan (f / 2))
fromFOV x f = 2 * atan (x / (2 * f))
mkIsoFOV x = iso (fromFOV x) (toFOV x) . from radians


