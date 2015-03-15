module Graphics.Camera (
    module Graphics.Camera,
    module Graphics.Camera.Angle,
    module Graphics.Camera.Classes,
    module Linear,
    OrthoCam, Cam
    ) where

import Control.Applicative

import Control.Lens
import Linear

import Graphics.Camera.Angle
import Graphics.Camera.Classes
import Graphics.Camera.Internal

-- | Given a line segment in viewport coordinates, compute an approximate camera rotation
viewerRotation :: (Camera3D c) => c a -> V2 a -> V2 a -> Quaternion a
viewerRotation cam vfrom vto = undefined

-- | Given a line segment in viewport coordinates, compute an approximate camera translation
viewerMovement :: (Camera3D c) => c a -> V2 a -> V2 a -> V3 a
viewerMovement cam vfrom vto = undefined

-- | Convert a point in viewport coordinates into a world-space vector pointing away from the camera
viewVector :: (Epsilon a, Floating a, Camera3D c) => c a -> V2 a -> V3 a
viewVector cam (V2 x y) = normalize . normalizePoint $ cam^.invCameraMatrix !* point (V3 x y 0)

-- | Convert a world-space vector into viewport coordinates if it's visible. 
--   Vectors pointing backwards (behind the camera) will be treated equivalent to
--   their negation. Vectors orthogonal to the camera view will produce infinity
--   or NaN coordinates.
vectorView :: (Epsilon a, Floating a, Camera3D c) => c a -> V3 a -> V2 a 
vectorView cam v = view _xy . normalizePoint $ cam^.cameraMatrix !* vector v

-- | Given a point in viewport coordinates, rotate the camera to center on it. 
centerView :: (Epsilon a, Ord a, Floating a, Camera3D c) => c a -> V2 a -> c a
centerView cam = centerOn cam . viewVector cam

-- | Given a rectangle in viewport coordinates, rotate the camera to center on 
--   it and zoom in as much as possible
centerZoomView :: (Ord a, Num a, Camera3D c) => c a -> (V2 a, V2 a) -> c a
centerZoomView cam (V2 x1 y1, V2 x2 y2) = undefined
  where
    newTL = V2 (max 0 $ min x1 x2) (max 0 $ min y1 y2)
    newBR = V2 (min (cam^.viewWidth) $ max x1 x2) (min (cam^.viewHeight) $ max y1 y2)

-- | Given a box in world-space coordinates, rotate the camera to center on it
--   and zoom in as much as possible
centerZoomOn :: (Camera3D c) => c a -> (V3 a, V3 a) -> c a
centerZoomOn cam box = undefined

-- | Given a point in world-space coordinates, rotate the camera to center on it
centerOn :: (Camera3D c) => c a -> V3 a -> c a
centerOn cam box = undefined

-- | Given a box in world-space coordinates, calculate how large it would be in viewport coordinates
viewerSize :: (Ord a, Epsilon a, Floating a, Camera3D c) => c a -> (V3 a, V3 a) -> (V2 a, V2 a)
viewerSize cam (V3 x1 y1 z1, V3 x2 y2 z2) = (V2 xmin ymin, V2 xmax ymax)
  where corners = map (vectorView cam) $ V3 <$> [x1, x2] <*> [y1, y2] <*> [z1, z2]
        xmin = minimum $ toListOf (each._x) corners
        ymin = minimum $ toListOf (each._y) corners
        xmax = maximum $ toListOf (each._x) corners
        ymax = maximum $ toListOf (each._y) corners
        

-- | Get the world-space bounds of the camera's view as a 3D mesh
viewSpace :: (Camera3D c) => c a -> [[V3 a]]
viewSpace cam = undefined




