{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Graphics.Camera.Classes where

import Control.Applicative
import Data.Foldable
import Data.Maybe
import Data.Data
import Data.Typeable
import GHC.Exts

import Control.Lens
import Linear hiding (distance)

import Graphics.Camera.Internal
import Graphics.Camera.Angle
import Graphics.Camera.Types


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


instance Camera OrthoCam where
    viewArea = ocamBaseCamera.bcamViewport

instance Camera Cam where
    viewArea = pcamBaseCamera.bcamViewport

instance (Camera c) => Camera (Jib c) where
    viewArea = jibCamera.viewArea

instance (Camera c) => Camera (Gimbal c) where
    viewArea = gimbalCamera.viewArea




-- | The @Camera3D@ class is an interface to cameras that exist in a 3D scene.
-- 
--    __Note:__ The result of setting camera properties to values that do not 
--    satisfy the requirements specified for each field is undefined behavior.
--    In particular, this may result in throwing exceptions and/or violating
--    the lens laws.
class (Camera c) => Camera3D c where    
    -- | The base coordinate system for an unrotated camera.
    coordinateSystem :: (Num a) => Lens' (c a) (M33 a)
    
    -- | The rightward axis, projected as the viewport positive X direction
    --
    --   * This is almost always @('V3' 1 0 0)@.
    rightward :: (Num a) => Lens' (c a) (V3 a)
    rightward = coordinateSystem._x
    
    -- | The upward axis, projected as the viewport positive Y direction
    --
    --   * This is conventionally @('V3' 0 1 0)@ in 3D graphics programming, 
    --     mapping the positive Y axis to the positive Y axis.
    --   * This is conventionally @('V3' 0 0 1)@ in many 3D modeling 
    --     applications (e.g., Blender), so that the Z axis is vertical and the
    --     XY plane is horizontal.
    upward :: (Num a) => Lens' (c a) (V3 a)
    upward = coordinateSystem._y
    
    -- | The forward axis, projected as viewport positive depth
    --
    --    * This is conventionally the Z axis in 3D graphics programming, 
    --      either @('V3' 0 0 1)@ for a left-handed coordinate system or 
    --      @('V3' 0 0 (-1))@ for right-handed.
    --   * This is conventionally @('V3' 0 1 0)@ in many 3D modeling 
    --     applications (e.g., Blender), so that the Z axis is vertical and the
    --     XY plane is horizontal.
    forward :: (Num a) => Lens' (c a) (V3 a)
    forward = coordinateSystem._z
    
    -- | Extract a transformation matrix from world space to view space.
    --
    -- For 3D cameras:
    -- @
    -- 'cameraMatrix' c ≡ 'projMatrix' c '!*!' 'viewMatrix' c
    -- @
    cameraMatrix :: (Epsilon a, RealFloat a) => Getter (c a) (M44 a)
    cameraMatrix = to $ \c -> c^.projMatrix !*! c^.viewMatrix
    
    -- | Extract a transformation matrix from view space to world space.
    --
    -- For 3D cameras:
    -- @
    -- 'invCameraMatrix' c ≡ 'invViewMatrix' c '!*!' 'invProjMatrix' c
    -- @
    invCameraMatrix :: (Epsilon a, RealFloat a) => Getter (c a) (M44 a)
    invCameraMatrix = to $ \c -> c^.invViewMatrix !*! c^.invProjMatrix
    
    -- | Extract a transformation matrix from world space to the camera's local
    --   object space. 
    viewMatrix :: (Epsilon a, RealFloat a) => Getter (c a) (M44 a)
    
    -- | Extract a transformation matrix from the camera's local object space
    --   to world space.
    invViewMatrix :: (Epsilon a, RealFloat a) => Getter (c a) (M44 a)
    
    -- | Extract a transformation matrix from the camera's local object space
    --   to view space.
    projMatrix :: (Epsilon a, RealFloat a) => Getter (c a) (M44 a)
    
    -- | Extract a transformation matrix from view space to the camera's local 
    --   object space.
    invProjMatrix :: (Epsilon a, RealFloat a) => Getter (c a) (M44 a)

    
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
        \c -> mkTransformationMat (inv33 $ c^.coordinateSystem) 0
          !*! inverseOrtho (c^.leftClip)   (c^.rightClip) 
                           (c^.bottomClip) (c^.topClip)
                           (c^.nearLimit)  (c^.farLimit)
    rangeLimit = ocamBaseCamera.bcamViewRange
    position = ocamBaseCamera.bcamPosition
    orientation = ocamBaseCamera.bcamOrientation

instance Camera3D Cam where
    coordinateSystem = pcamBaseCamera.bcamCoords.iso (_z %~ negate) (_z %~ negate)
    viewMatrix = to $ \c -> mkTransformation (c^.orientation) (c^.position)
    invViewMatrix = to $ \c -> mkTransformation (c^.orientation & _ijk.each %~ negate) (c^.position & each %~ negate)
    projMatrix = to $ 
        \c -> perspective (c^.fovVertical.radians) (c^.viewAspect) (c^.nearLimit) (c^.farLimit)
          !*! mkTransformationMat (c^.coordinateSystem) 0
    invProjMatrix = to $ 
        \c -> mkTransformationMat (inv33 $ c^.coordinateSystem) 0
          !*! inversePerspective (c^.fovVertical.radians) (c^.viewAspect) (c^.nearLimit) (c^.farLimit) 
    rangeLimit = pcamBaseCamera.bcamViewRange
    position = pcamBaseCamera.bcamPosition
    orientation = pcamBaseCamera.bcamOrientation

instance (Camera3D c) => Camera3D (Jib c) where
    coordinateSystem = jibCamera.coordinateSystem
    viewMatrix = to $ \c -> mkTransformationMat identity (negate <$> c^.displacement) !*! c^.jibCamera.viewMatrix 
    invViewMatrix = to $ \c -> c^.jibCamera.invViewMatrix !*! mkTransformationMat identity (c^.displacement)
    projMatrix = jibCamera.projMatrix
    invProjMatrix = jibCamera.invProjMatrix
    position = jibCamera.position
    orientation = jibCamera.orientation
    rangeLimit = jibCamera.rangeLimit

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
    -- | Create a perspective view camera.
    perspectiveCamera :: (RealFloat a)
                      => V2 a               -- ^ Viewport size
                      -> CoordinateSystem a -- ^ Coordinate system
                      -> a                  -- ^ Focal length
                      -> a                  -- ^ Near clipping plane
                      -> a                  -- ^ Far clipping plane
                      -> c a

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
    --   and focal length /f/, field of view is given by /fov = 2 arctan(d \/ 2f)/.
    --
    --   Both dimensions should be positive, non-zero angles.
    --   
    --   * 'fovHorizontal' and 'fovVertical' are the X and Y components
    --   * Adjusts 'focalArea' to match the new FOV
    --   * Leaves 'focalLength' unchanged
    fieldOfView :: (Floating a) => Lens' (c a) (V2 (Angle a))
    fieldOfView = lens get set
      where
        get c = toFOV <$> effectiveFocalArea c <*> pure (c^.focalLength)
        set c sz = c & focalArea .~ Just (toSensorSize (c^.focalLength) <$> sz)
    
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


instance PerspectiveCamera Cam where
    perspectiveCamera v cd fl near far = PCam (BCam coordsMatrix v (near, far) 0 1) Nothing fl
      where coordsMatrix = case cd of
                VerticalZ -> V3 (V3 1 0 0) (V3 0 0 1) (V3 0 1 0)
                ZForward  -> V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
                ZBackward -> V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 (-1))
                OtherCoords m -> m
    focalArea = pcamFocalArea
    focalLength = pcamFocalLength

instance (PerspectiveCamera c) => PerspectiveCamera (Jib c) where
    perspectiveCamera v cd fl near far = newJib $ perspectiveCamera v cd fl near far
    focalArea = jibCamera.focalArea
    focalLength = jibCamera.focalLength

instance (PerspectiveCamera c) => PerspectiveCamera (Gimbal c) where
    perspectiveCamera v cd fl near far = newGimbal $ perspectiveCamera v cd fl near far
    focalArea = gimbalCamera.focalArea
    focalLength = gimbalCamera.focalLength

-- | Compute the effective focal area for a camera, either an explicitly set
--   value or the default.
effectiveFocalArea :: (PerspectiveCamera c, Floating a) => c a -> V2 a
effectiveFocalArea c = fromMaybe ((c & viewDiagonal *~ 0.042)^.viewArea) (c^.focalArea)

-- | The @OrthographicCamera@ class is an interface to orthographic projection
--   cameras.
--
--    __Note:__ The result of setting camera properties to values that do not 
--    satisfy the requirements specified for each field is undefined behavior.
--    In particular, this may result in throwing exceptions and/or violating
--    the lens laws.
class (Camera3D c) => OrthographicCamera c where
    
    -- | Create an orthographic camera with a view size and depth range. World
    --   space coordinates are assumed to match the viewport coordinates.
    orthoCamera :: (RealFloat a) 
                => V2 a -- ^ Viewport size
                -> a    -- ^ Near clipping plane
                -> a    -- ^ Far clipping plane
                -> c a
    
    -- | Create an orthographic camera from a view size and six clipping planes
    orthoFromPlanes :: (RealFloat a) 
                    => V2 a -- ^ Viewport size
                    -> a    -- ^ Left
                    -> a    -- ^ Right
                    -> a    -- ^ Bottom
                    -> a    -- ^ Top
                    -> a    -- ^ Near
                    -> a    -- ^ Far
                    -> c a
    
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


instance OrthographicCamera OrthoCam where
    orthoCamera v@(V2 w h) near far = OCam (BCam identity v (near, far) 0 1) (0, w) (0, h)
    orthoFromPlanes v l r b t n f = OCam (BCam identity v (n, f) 0 1) (l, r) (b, t)
    horizontalRange = ocamHRange
    verticalRange = ocamVRange

instance (OrthographicCamera c) => OrthographicCamera (Jib c) where
    orthoCamera v near far = newJib $ orthoCamera v near far 
    orthoFromPlanes v l r b t n f = newJib $ orthoFromPlanes v l r b t n f
    horizontalRange = jibCamera.horizontalRange
    verticalRange = jibCamera.verticalRange

instance (OrthographicCamera c) => OrthographicCamera (Gimbal c) where
    orthoCamera v near far = newGimbal $ orthoCamera v near far 
    orthoFromPlanes v l r b t n f = newGimbal $ orthoFromPlanes v l r b t n f
    horizontalRange = gimbalCamera.horizontalRange
    verticalRange = gimbalCamera.verticalRange



-- | Jib operations apply an offset to a camera in its local coordinate space.
class (Camera3D c) => JibCamera c where
    -- | Displacement of camera relative to its base position.
    displacement :: Lens' (c a) (V3 a)
    
    -- | Displacement along the camera's forward axis.
    distance :: (Num a) => Lens' (c a) a
    distance = lens 
        (\c -> dot (c^.displacement) (c^.forward))
        (\c d -> c & displacement +~ (pure d * c^.forward) - (c^.displacement * abs (c^.forward)))

instance (Camera3D c) => JibCamera (Jib c) where
    displacement = jibDisplacement

instance (JibCamera c) => JibCamera (Gimbal c) where
    displacement = gimbalCamera.displacement


-- | Gimbal operations apply a sequence of rotations to a camera in its local 
--   coordinate space.
class (Camera3D c) => GimbalCamera c where
    -- | Rotation angle in the horizontal plane
    heading :: Lens' (c a) (Angle a)
    
    -- | Rotation around the camera's horizontal axis
    elevation :: Lens' (c a) (Angle a)
    
    -- | Rotation around the camera's view direction
    roll :: Lens' (c a) (Angle a)
    
    -- | All three rotations combined
    gimbalRotation :: (Epsilon a, RealFloat a) => Getter (c a) (Quaternion a)
    gimbalRotation = to $ 
        \c -> axisAngle (c^.forward)   (c^.roll.radians) 
            * axisAngle (c^.rightward) (c^.elevation.radians) 
            * axisAngle (c^.upward)    (c^.heading.radians)
    
    -- | The inverse of 'gimbalRotation'.
    invGimbalRotation :: (Epsilon a, RealFloat a) => Getter (c a) (Quaternion a)
    invGimbalRotation = to $ 
        \c -> axisAngle (c^.forward)   (- c^.roll.radians) 
            * axisAngle (c^.rightward) (- c^.elevation.radians) 
            * axisAngle (c^.upward)    (- c^.heading.radians)

instance (GimbalCamera c) => GimbalCamera (Jib c) where
    heading = jibCamera.heading
    elevation = jibCamera.elevation
    roll = jibCamera.roll

instance (Camera3D c) => GimbalCamera (Gimbal c) where
    heading = gimbalHeading
    elevation = gimbalElevation
    roll = gimbalRoll



