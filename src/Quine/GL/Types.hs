--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.GL.Types
  ( 
  -- * Vectors
    Vec2, Vec3, Vec4
  , BVec2, BVec3, BVec4
  , DVec2, DVec3, DVec4
  , IVec2, IVec3, IVec4
  , UVec2, UVec3, UVec4
  -- * Matrices
  , Mat2, Mat3, Mat4
  , DMat2, DMat3, DMat4
  , DMat2x3, DMat2x4, DMat3x2
  , DMat3x4, DMat4x2, DMat4x3
  , Mat2x3, Mat2x4, Mat3x2
  , Mat3x4, Mat4x2, Mat4x3
  ) where

import Data.Int
import Data.Word
import Linear

type BVec2 = V2 Bool
type BVec3 = V3 Bool
type BVec4 = V4 Bool
type DMat2 = M22 Double
type DMat2x3 = M23 Double
type DMat2x4 = M24 Double
type DMat3 = M33 Double
type DMat3x2 = M32 Double
type DMat3x4 = M34 Double
type DMat4 = M44 Double
type DMat4x2 = M42 Double
type DMat4x3 = M43 Double
type DVec2 = V2 Double
type DVec3 = V3 Double
type DVec4 = V4 Double
type IVec2 = V2 Int32
type IVec3 = V3 Int32
type IVec4 = V4 Int32
type Mat2 = M22 Float
type Mat2x3 = M23 Float
type Mat2x4 = M24 Float
type Mat3 = M33 Float
type Mat3x2 = M32 Float
type Mat3x4 = M34 Float
type Mat4 = M44 Float
type Mat4x2 = M42 Float
type Mat4x3 = M43 Float
type UVec2 = V2 Word32
type UVec3 = V3 Word32
type UVec4 = V4 Word32
type Vec2 = V2 Float
type Vec3 = V3 Float
type Vec4 = V4 Float
