#ifndef INCLUDED_CAMERA_H
#define INCLUDED_CAMERA_H 1

#include "geometry/frustum.h"

struct Camera {
  Frustum Frustum;
  mat4 projection, inverseProjection, modelView, inverseModelView;
  float fovy, aspectRatio, near, far;
};

#endif
