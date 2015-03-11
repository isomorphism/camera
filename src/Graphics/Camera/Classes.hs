{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Graphics.Camera.Classes where

import Data.Typeable
import GHC.Exts
import Control.Lens
import Linear
import Graphics.Camera.Angle


-- | The @Camera@ class is a basic common interface to different cameras.
--
--    __Note:__ The result of setting camera properties to values that do not 
--    satisfy the requirements specified for each field is undefined behavior.
--    In particular, this may result in throwing exceptions and/or violating
--    the lens laws.
class Camera c where
    -- | Extract a transformation matrix from world space to view space.
    --
    -- @
    -- 'cameraMatrix' c ≡ 'projMatrix' c '!*!' 'viewMatrix' c
    -- @
    cameraMatrix :: (Floating a) => c a -> M44 a
    
    -- | Extract a transformation matrix from view space to world space.
    --
    -- @
    -- 'invCameraMatrix' c ≡ 'invViewMatrix' c '!*!' 'invProjMatrix' c
    -- @
    invCameraMatrix :: (Floating a) => c a -> M44 a
    
    -- | Extract a transformation matrix from world space to the camera's local
    --   object space. 
    viewMatrix :: (Floating a) => c a -> M44 a
    
    -- | Extract a transformation matrix from the camera's local object space
    --   to world space.
    invViewMatrix :: (Floating a) => c a -> M44 a
    
    -- | Extract a transformation matrix from the camera's local object space
    --   to view space.
    projMatrix :: (Floating a) => c a -> M44 a
    
    -- | Extract a transformation matrix from view space to the camera's local 
    --   object space.
    invProjMatrix :: (Floating a) => c a -> M44 a
    
    -- | The dimensions of the image seen by the camera, in view space 
    --   coordinates. Generally this will be the pixel size of the rendered 
    --   image.
    --
    --   Both dimensions should be positive, non-zero values.
    --
    --   * 'viewWidth' and 'viewHeight' are the X and Y components
    --   * 'viewAspect' is the aspect ratio W/H
    --   * 'viewDiagonal' is the diagonal size of the view area
    viewArea     :: Lens' (c a) (V2 a)
    
    -- | The width of the image seen by the camera, in view space coordinates.
    --
    --   @viewWidth@ should be a positive, non-zero value.
    --
    --   * Accesses the X component of 'viewArea'
    --   * Leaves 'viewHeight' unchanged
    --   * Adjusts 'viewDiagonal' and 'viewAspect' to match the new width.
    viewWidth    :: Lens' (c a) a
    
    -- | The height of the image seen by the camera, in view space coordinates.
    --
    --   @viewHeight@ should be a positive, non-zero value.
    --
    --   * Accesses the Y component of 'viewArea'
    --   * Leaves 'viewWidth' unchanged
    --   * Adjusts 'viewDiagonal' and 'viewAspect' to match the new width.
    viewHeight   :: Lens' (c a) a
    
    -- | The aspect ratio of the image seen by the camera. The ratio is W/H, 
    --   i.e. values > 1.0 are wide and short, values < 1.0 are tall and narrow.
    --
    --   @viewAspect@ should be a positive, non-zero value.
    --
    --   * Leaves 'viewDiagonal' unchanged
    --   * Adjusts 'viewArea', 'viewWidth', and 'viewHeight' to have the new
    --     aspect ratio with the same diagonal size
    viewAspect   :: (Floating a) => Lens' (c a) a
    
    -- | The diagonal size of the image seen by the camera, in view space 
    --   coordinates.
    --
    --   @viewDiagonal@ should be a positive, non-zero value.
    --
    --   * Leaves 'viewAspect' unchanged
    --   * Scales 'viewArea', 'viewWidth', and 'viewHeight' to match the new
    --     diagonal size with the same aspect ratio
    viewDiagonal :: (Floating a) => Lens' (c a) a
    
    -- | The depth range seen by the camera, in view space coordinates, as 
    --   @(near, far)@. Anything outside this range after projection to view 
    --   space will be ignored. 
    --
    --   __Note:__ When using a depth buffer for rendering, the possible depth 
    --   values will generally be distributed over this range, so avoid setting 
    --   the range much larger than you need on pain of depth test bugs.
    --
    --   Both components should be non-zero, and 'nearLimit' should be less than
    --   'farLimit'.
    --
    --   * 'nearLimit' is the near clipping plane.
    --   * 'farLimit' is the far clipping plane.
    rangeLimit   :: Lens' (c a) (a, a)
    
    -- | The near clipping plane, in view space coordinates.
    --
    --   * Accesses the first component of 'rangeLimit'
    nearLimit    :: Lens' (c a) a
    
    -- | The far clipping plane, in view space coordinates.
    --
    --   * Accesses the second component of 'rangeLimit'
    farLimit     :: Lens' (c a) a
    
    -- | The position of the camera, in world space coordinates.
    position     :: Lens' (c a) (V3 a)
    
    -- | The world space orientation of the camera, as a unit quaternion.
    --
    -- 'orientation' should have a magnitude of 1.
    orientation  :: Lens' (c a) (Quaternion a)

-- | The @PerspectiveCamera@ class is an interface to perspective projection
--   cameras. Which seems kind of redundant, and /would/ be in real life, but
--   some properties don't really make sense on (non-physical) orthographic 
--   cameras.
class (Camera c) => PerspectiveCamera c where
    -- | The "focal length" of the virtual camera.
    --
    --   'focalLength' should be a positive, non-zero value.
    focalLength  :: (Floating a) => Lens' (c a) a
    
    -- | The "sensor" size of the virtual camera, relating FOV to focal length.
    --
    --   The default behavior will use 'viewArea' with an additional scaling 
    --   factor based on typical desktop monitor sizes &c. such that 
    --   'focalLength' is roughly the 35mm equivalent focal length for a 
    --   full-screen viewport. This means that:
    --   
    --   1. In a full-screen window, 'focalLength' will mean approximately the
    --      same thing that "focal length" does on many physical cameras.
    --   2. The apparent size of rendered objects will not change if the 
    --      viewport is resized.
    --
    --   If this is set to an explicit value, 'viewArea' will be ignored and the
    --   specified size will be used (treated as millimeters). This means that:
    --
    --   1. Setting 'focalArea' to @Just (V2 36 24)@ will make 'focalLength'
    --      behave as the 35mm equivalent value. (Standard 35mm film, of course,
    --      having an image size of 36mm x 24mm. What /else/ would it be?)
    --   2. The rendered scene will not change if the viewport is resized,
    --      and will be distorted if the aspect ratios don't match.
    focalArea    :: Lens' (c a) (Maybe (V2 a))

    -- | The angular field of view of the virtual camera. For "sensor" size /d/ 
    --   and focal length /f/, field of view is given by /fov = 2arctan(d / 2f)/.
    --
    --   Both dimensions should be positive, non-zero angles.
    --   
    --   * 'fovHorizontal' and 'fovVertical' are the X and Y components
    fieldOfView   :: (Floating a) => c a -> V2 (Angle a)
    
    -- | The angular field of view of the virtual camera along the horizontal 
    --   view axis. 
    --
    --   * Accesses the X component of 'fieldOfView'
    fovHorizontal :: (Floating a) => Lens' (c a) (Angle a)

    -- | The angular field of view of the virtual camera along the vertical 
    --   view axis. 
    --
    --   * Accesses the Y component of 'fieldOfView'
    fovVertical   :: (Floating a) => Lens' (c a) (Angle a)


-- | Compute the focal length for a given sensor size and FOV angle.
fromFOV :: (Floating a) => a -> Angle a -> a
fromFOV d a = d / (2 * tan (a^.radians / 2))

-- | Compute the field of view for a given sensor size and focal length.
toFOV :: (Floating a) => a -> a -> Angle a
toFOV d f = angleFrom radians $ 2 * atan (d / (2 * f))

-- | For a constant sensor size we have an isomorphism between FOV and focal length
isoFOV :: (Floating a) => a -> Iso' a (Angle a)
isoFOV x = iso (toFOV x) (fromFOV x) -- . from radians

data BaseCamera a = BCam
    { _bcamViewport    :: V2 a
    , _bcamViewRange   :: (a, a)
    , _bcamPosition    :: V3 a
    , _bcamOrientation :: Quaternion a
    } deriving (Eq, Ord, Typeable)

