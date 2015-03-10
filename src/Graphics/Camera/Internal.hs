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

-- | Orthographic projection camera
newtype OrthoCam a = OCam { _ocamBaseCamera :: BaseCamera a }
  deriving (Eq, Ord, Typeable)

makeLenses ''OrthoCam

instance Camera OrthoCam where
    -- TODO

-- | Perspective projection camera
data Cam a = PCam
    { _pcamBaseCamera  :: BaseCamera a
    , _pcamFocalLength :: a
    , _pcamFocus :: a
    } deriving (Eq, Ord, Typeable)

makeLenses ''Cam

instance Camera Cam where
    -- TODO

instance PerspectiveCamera Cam where
    -- TODO



