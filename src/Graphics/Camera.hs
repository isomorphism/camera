module Graphics.Camera (
    module Graphics.Camera,
    module Graphics.Camera.Angle,
    module Graphics.Camera.Classes,
    module Linear,
    OrthoCam, Cam
    ) where

import Linear

import Graphics.Camera.Angle
import Graphics.Camera.Classes
import Graphics.Camera.Internal

-- | Given a line segment in viewport coordinates, compute an approximate camera rotation
viewerRotation :: (Camera c) => c a -> V2 a -> V2 a -> Quaternion a
viewerRotation cam vfrom vto = undefined

-- | Given a line segment in viewport coordinates, compute an approximate camera translation
viewerMovement :: (Camera c) => c a -> V2 a -> V2 a -> V3 a
viewerMovement cam vfrom vto = undefined

-- | Convert a point in viewport coordinates into a world-space vector pointing away from the camera
viewVector :: (Camera c) => c a -> V2 a -> V3 a
viewVector cam vpt = undefined

-- | Convert a world-space vector into viewport coordinates if it's visible
vectorView :: (Camera c) => c a -> V3 a -> Maybe (V2 a)
vectorView cam v = undefined

-- | Given a rectangle in viewport coordinates, center the camera on it and zoom in as much as possible
zoomView :: (Camera c) => c a -> (V2 a, V2 a) -> c a
zoomView cam (tl, br) = undefined

-- | Given a box in world-space coordinates, rotate the camera to center on it
centerOn :: (Camera c) => c a -> (V3 a, V3 a) -> c a
centerOn cam box = undefined

-- | Given a box in world-space coordinates, calculate how large it would be in viewport coordinates
viewerSize :: (Camera c) => c a -> (V3 a, V3 a) -> (V2 a, V2 a)
viewerSize cam box = undefined

-- | Get the world-space bounds of the camera's view as a 3D mesh
viewSpace :: (Camera c) => c a -> [[V3 a]]
viewSpace cam = undefined


