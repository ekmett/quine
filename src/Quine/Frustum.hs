module Quine.Frustum
  ( Frustum(..)
  ) where

data Frustum = Frustum 
  !Plane !Plane !Plane !Plane !Plane !Plane
  !Vec3 !Vec3 !Vec3 !Vec3 !Vec3 !Vec3 !Vec3 !Vec3 

-- | @buildFrustum origin direction nearZ farZ fovy aspectRatio@
buildFrustum :: Vec3 -> Vec3 -> Vec3 -> Float -> Float -> Float -> Float -> Frustum
buildFrustum origin dir up near far fovy aspect = undefined
  where
    t = tan (fovy*0.5)
    nc = origin + near*^dir
    fc = origin + far*^dir
    nh = t * near
    fh = t * far
    nw = nh * aspect
    fw = fh * aspect

planes :: Frustum -> [Plane]
planes (Frustum a b c d e f _ _ _ _ _ _ _ _) = [a,b,c,d,e,f]

points :: Frustum -> [Vec3]
points (Frustum _ _ _ _ _ _ a b c d e f g h) = [a,b,c,d,e,f,g,h]

mix :: V3 a -> V3 a -> V3 a
mix (V3 a b c) (V3 d e f) = V3 <$> [a,d] <*> [b,e] <*> [c,f] 

instance OverlapsBox Frustum where
  overlapsBox fm (Box l h) = not $
      any (\p -> all (\q -> signedDistance p q < 0) (mix l h)) ps
   || all (\p -> p^._x > h^._x) qs
   || all (\p -> p^._y > h^._y) qs
   || all (\p -> p^._z > l^._z) qs
   || all (\p -> p^._x < l^._x) qs
   || all (\p -> p^._y < l^._y) qs
   || all (\p -> p^._z < l^._z) qs
   where ps = planes fm
         qs = points fm
