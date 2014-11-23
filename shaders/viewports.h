#ifndef INCLUDED_VIEWPORTS_H
#define INCLUDED_VIEWPORTS_H 1

// (c) 2014 Edward Kmett

#include "camera.h"

// the maximum number of viewports we'll use
//
// used in layout() context, so we can't use a const int.
#define MAX_VIEWPORTS 16

// --------------------------------------------------------------------------------
// Uniforms
// --------------------------------------------------------------------------------

uniform int viewportCount;                     // number of actual viewports to use
uniform Camera viewportCamera[MAX_VIEWPORTS]; // the set of cameras

// --------------------------------------------------------------------------------
// Geometry -> Fragment
// --------------------------------------------------------------------------------

#ifdef GEOMETRY
flat GEOMETRY int viewportIndex; // the viewport index we're using now
flat GEOMETRY vec3 eyePosition;  // world-space eye position for this camera
GEOMETRY vec4 eyeDeviceCoord;    // gl_Position for the fragment shader in NDC
GEOMETRY vec3 eyeDirection;      // world-space ray direction for the current pixel
GEOMETRY vec3 eyeForward;        // world-space ray direction going forward for the camera
#endif // GEOMETRY

#endif // INCLUDED_VIEWPORTS_H
