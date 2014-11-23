#ifndef INCLUDED_CAMERA_H
#define INCLUDED_CAMERA_H 1

struct Camera {
  mat4 projection, modelView;
  float fovy, aspectRatio, near, far;
};

#endif
