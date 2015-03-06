{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Camera.Internal where

import Data.Typeable

import Control.Lens
import Linear

import Graphics.Camera.Angle
import Graphics.Camera.Classes

data BaseCamera a = BCam
    { _bcamViewport    :: V2 a
    , _bcamViewRange   :: (a, a)
    , _bcamPosition    :: V3 a
    , _bcamOrientation :: Quaternion a
    } deriving (Eq, Ord, Typeable)

makeLenses ''BaseCamera

instance Camera BaseCamera where
    type CamView BaseCamera  = Fractional
    viewArea = bcamViewport
    viewRange = bcamViewRange
    position = bcamPosition
    orientation = bcamOrientation


newtype OrthoCam a = OCam { _ocamBaseCamera :: BaseCamera a }
  deriving (Eq, Ord, Typeable)

makeLenses ''OrthoCam

instance Camera OrthoCam where
    type CamView OrthoCam = Fractional
    viewArea = ocamBaseCamera . bcamViewport
    viewRange = ocamBaseCamera . bcamViewRange
    position = ocamBaseCamera . bcamPosition
    orientation = ocamBaseCamera . bcamOrientation


data Cam a = PCam
    { _pcamBaseCamera  :: BaseCamera a
    , _pcamFocalLength :: a
    , _pcamFocus :: a
    } deriving (Eq, Ord, Typeable)

makeLenses ''Cam

instance Camera Cam where
    type CamView Cam = Floating
    viewArea = pcamBaseCamera . bcamViewport
    viewRange = pcamBaseCamera . bcamViewRange
    position = pcamBaseCamera . bcamPosition
    orientation = pcamBaseCamera . bcamOrientation

instance ProjCamera Cam where
    focalLength = pcamFocalLength
    focus = pcamFocus

data Targetted c a = TCam
    { _tcamCamera         :: c a
    , _tcamTargetPosition :: V3 a
    , _tcamUpVector       :: V3 a
    , _tcamForwardVector  :: V3 a
    } deriving (Eq, Ord, Typeable)

makeLenses ''Targetted

instance (Camera c) => Camera (Targetted c) where
    type CamView (Targetted c) = CamView c
    viewArea = tcamCamera . viewArea
    viewRange = tcamCamera . viewRange
    position = tcamCamera . position
    orientation = tcamCamera . orientation

instance (CamView (Targetted c) ~ CamView c, Camera (Targetted c), ProjCamera c) => ProjCamera (Targetted c) where
    focalLength = tcamCamera . focalLength

instance (Camera c) => TargettedCamera Targetted c where
    untarget = view tcamCamera
    targetPosition = tcamTargetPosition
    upVector = tcamUpVector
    forwardVector = tcamForwardVector

