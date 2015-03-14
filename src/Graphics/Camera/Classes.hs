{-# LANGUAGE RankNTypes #-}
module Graphics.Camera.Classes where

import Control.Applicative
import Data.Maybe
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
    -- | The dimensions of the image seen by the camera, in view space 
    --   coordinates. Generally this will be the pixel size of the rendered 
    --   image.
    --
    --   Both dimensions should be positive, non-zero values.
    --
    --   * 'viewWidth' and 'viewHeight' are the X and Y components
    --   * 'viewAspect' is the aspect ratio W/H
    --   * 'viewDiagonal' is the diagonal size of the view area
    viewArea :: Lens' (c a) (V2 a)
    
    -- | The width of the image seen by the camera, in view space coordinates.
    --
    --   @viewWidth@ should be a positive, non-zero value.
    --
    --   * Accesses the X component of 'viewArea'
    --   * Leaves 'viewHeight' unchanged
    --   * Adjusts 'viewDiagonal' and 'viewAspect' to match the new width.
    viewWidth :: Lens' (c a) a
    viewWidth = viewArea._x
    
    
    -- | The height of the image seen by the camera, in view space coordinates.
    --
    --   @viewHeight@ should be a positive, non-zero value.
    --
    --   * Accesses the Y component of 'viewArea'
    --   * Leaves 'viewWidth' unchanged
    --   * Adjusts 'viewDiagonal' and 'viewAspect' to match the new width.
    viewHeight :: Lens' (c a) a
    viewHeight = viewArea._y
    
    -- | The aspect ratio of the image seen by the camera. The ratio is W/H, 
    --   i.e. values > 1.0 are wide and short, values < 1.0 are tall and narrow.
    --
    --   @viewAspect@ should be a positive, non-zero value.
    --
    --   * Leaves 'viewDiagonal' unchanged
    --   * Adjusts 'viewArea', 'viewWidth', and 'viewHeight' to have the new
    --     aspect ratio with the same diagonal size
    viewAspect :: (Floating a) => Lens' (c a) a
    viewAspect = lens (\c -> c^.viewWidth / c^.viewHeight) (\c a -> c & viewArea .~ V2 a 1 & viewDiagonal .~ c^.viewDiagonal)
    
    -- | The diagonal size of the image seen by the camera, in view space 
    --   coordinates.
    --
    --   @viewDiagonal@ should be a positive, non-zero value.
    --
    --   * Leaves 'viewAspect' unchanged
    --   * Scales 'viewArea', 'viewWidth', and 'viewHeight' to match the new
    --     diagonal size with the same aspect ratio
    viewDiagonal :: (Floating a) => Lens' (c a) a
    viewDiagonal = lens (\c -> sqrt $ (c^.viewWidth)^2 + (c^.viewHeight)^2) (\c d -> c & viewArea %~ fmap ((d / (c^.viewDiagonal)) *))

-- | The @Camera3D@ class is an interface to cameras that exist in a 3D scene.
-- 
--    __Note:__ The result of setting camera properties to values that do not 
--    satisfy the requirements specified for each field is undefined behavior.
--    In particular, this may result in throwing exceptions and/or violating
--    the lens laws.
class (Camera c) => Camera3D c where    
        -- | Extract a transformation matrix from world space to view space.
    --
    -- For 3D cameras:
    -- @
    -- 'cameraMatrix' c ≡ 'projMatrix' c '!*!' 'viewMatrix' c
    -- @
    cameraMatrix :: (Floating a) => c a -> M44 a
    cameraMatrix c = projMatrix c !*! viewMatrix c
    
    -- | Extract a transformation matrix from view space to world space.
    --
    -- For 3D cameras:
    -- @
    -- 'invCameraMatrix' c ≡ 'invViewMatrix' c '!*!' 'invProjMatrix' c
    -- @
    invCameraMatrix :: (Floating a) => c a -> M44 a
    invCameraMatrix c = invViewMatrix c !*! invProjMatrix c
    
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
    rangeLimit :: Lens' (c a) (a, a)
    
    -- | The near clipping plane, in view space coordinates.
    --
    --   * Accesses the first component of 'rangeLimit'
    nearLimit :: Lens' (c a) a
    nearLimit = rangeLimit._1
    
    -- | The far clipping plane, in view space coordinates.
    --
    --   * Accesses the second component of 'rangeLimit'
    farLimit :: Lens' (c a) a
    farLimit = rangeLimit._2
    
    -- | The position of the camera, in world space coordinates.
    position :: Lens' (c a) (V3 a)
    
    -- | The world space orientation of the camera, as a unit quaternion.
    --
    -- 'orientation' should have a magnitude of 1.
    orientation :: Lens' (c a) (Quaternion a)

-- | The @PerspectiveCamera@ class is an interface to perspective projection
--   cameras. Which seems kind of redundant, and /would/ be in real life, but
--   some properties don't really make sense on (non-physical) orthographic 
--   cameras.
--
--    __Note:__ The result of setting camera properties to values that do not 
--    satisfy the requirements specified for each field is undefined behavior.
--    In particular, this may result in throwing exceptions and/or violating
--    the lens laws.
class (Camera3D c) => PerspectiveCamera c where
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
    --   and focal length /f/, field of view is given by /fov = 2 arctan(d / 2f)/.
    --
    --   Both dimensions should be positive, non-zero angles.
    --   
    --   * 'fovHorizontal' and 'fovVertical' are the X and Y components
    --   * Adjusts 'focalArea' to match the new FOV
    --   * Leaves 'focalLength' unchanged
    fieldOfView :: (Floating a) => Lens' (c a) (V2 (Angle a))
    fieldOfView = lens get set
      where
        defaultArea c = (c & viewDiagonal .~ 42)^.viewArea
        get c = V2 (toFOV w $ c^.focalLength) (toFOV h $ c^.focalLength)
          where V2 w h = fromMaybe (defaultArea c) (c^.focalArea)
        set c sz = c & focalArea .~ Just (toSensorSize (c^.focalLength) <$> sz)
    
    fovAspect :: (Floating a) => Getter (c a) a
    fovAspect = to $ \c -> c^.fovHorizontal.turns / c^.fovVertical.turns
    
    -- | The angular field of view of the virtual camera along the horizontal 
    --   view axis. 
    --
    --   * Accesses the X component of 'fieldOfView'
    --   * Adjusts 'focalArea' to match the new FOV
    --   * Leaves 'focalLength' unchanged
    fovHorizontal :: (Floating a) => Lens' (c a) (Angle a)
    fovHorizontal = fieldOfView._x

    -- | The angular field of view of the virtual camera along the vertical 
    --   view axis. 
    --
    --   * Accesses the Y component of 'fieldOfView'
    --   * Adjusts 'focalArea' to match the new FOV
    --   * Leaves 'focalLength' unchanged
    fovVertical :: (Floating a) => Lens' (c a) (Angle a)
    fovVertical = fieldOfView._y


-- | The @OrthographicCamera@ class is an interface to orthographic projection
--   cameras.
--
--    __Note:__ The result of setting camera properties to values that do not 
--    satisfy the requirements specified for each field is undefined behavior.
--    In particular, this may result in throwing exceptions and/or violating
--    the lens laws.
class (Camera3D c) => OrthographicCamera c where
    -- | The horizontal clipping planes. 
    -- 
    --   Both components should be non-zero, and 'leftClip' should be less than
    --   'rightClip'.
    --
    --   * 'leftClip' is the left clipping plane.
    --   * 'rightClip' is the right clipping plane.
    horizontalRange :: Lens' (c a) (a, a)
    
    -- | The left clipping plane, in view space coordinates.
    --
    --   * Accesses the first component of 'horizontalRange'
    leftClip :: Lens' (c a) a
    leftClip = horizontalRange._1
    
    -- | The right clipping plane, in view space coordinates.
    --
    --   * Accesses the second component of 'horizontalRange'
    rightClip :: Lens' (c a) a
    rightClip = horizontalRange._2
    
    -- | The vertical clipping planes. 
    -- 
    --   Both components should be non-zero, and 'bottomClip' should be less than
    --   'topClip'.
    --
    --   * 'bottomClip' is the bottom clipping plane.
    --   * 'topClip' is the top clipping plane.
    verticalRange :: Lens' (c a) (a, a)

    -- | The bottom clipping plane, in view space coordinates.
    --
    --   * Accesses the first component of 'verticalRange'
    bottomClip :: Lens' (c a) a
    bottomClip = verticalRange._1
    
    -- | The top clipping plane, in view space coordinates.
    --
    --   * Accesses the second component of 'verticalRange'
    topClip :: Lens' (c a) a
    topClip = verticalRange._2



-- | Compute the focal length for a given sensor size and FOV angle.
toFocalLength :: (Floating a) => a -> Angle a -> a
toFocalLength d a = d / (2 * tan (a^.radians / 2))

-- | Compute the sensor size for a focal length and FOV angle.
toSensorSize :: (Floating a) => a -> Angle a -> a
toSensorSize l a = l * (2 * tan (a^.radians / 2))

-- | Compute the field of view for a given sensor size and focal length.
toFOV :: (Floating a) => a -> a -> Angle a
toFOV d f = angleFrom radians $ 2 * atan (d / (2 * f))

-- | For a constant sensor size we have an isomorphism between FOV and focal length
isoFocalLengthFOV :: (Floating a) => a -> Iso' a (Angle a)
isoFocalLengthFOV sz = iso (toFOV sz) (toFocalLength sz)

-- | For a constant focal length we have an isomorphism between FOV and sensor size
isoSensorFOV :: (Floating a) => a -> Iso' a (Angle a)
isoSensorFOV fl = iso (flip toFOV fl) (toSensorSize fl)




