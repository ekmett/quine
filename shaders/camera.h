#ifndef INCLUDED_CAMERA_H
#define INCLUDED_CAMERA_H 1

#include "frustum.h"

struct camera {
  frustum frustum;
  mat44 projection, inverseProjection;
  mat44 modelView, inverseModelView;
};

#endif
