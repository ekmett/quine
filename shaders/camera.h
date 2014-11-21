#ifndef INCLUDED_CAMERA_H
#define INCLUDED_CAMERA_H 1

#include "frustum.h"

struct camera {
  frustum frustum;
  mat4 projection, inverseProjection, modelView, inverseModelView;
};

#endif
