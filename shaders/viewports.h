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

uniform int   viewportCount; // number of actual viewports to use

// TODO: Add UBO support so these can just go in one.
uniform mat4  viewportCameraProjection[MAX_VIEWPORTS];
uniform mat4  viewportCameraModelView[MAX_VIEWPORTS];
uniform float viewportCameraFovy[MAX_VIEWPORTS];
uniform float viewportCameraAspectRatio[MAX_VIEWPORTS];
uniform float viewportCameraNear[MAX_VIEWPORTS];
uniform float viewportCameraFar[MAX_VIEWPORTS];

// --------------------------------------------------------------------------------
// Geometry -> Fragment
// --------------------------------------------------------------------------------

#ifdef GEOMETRY
flat GEOMETRY int viewportIndex; // the viewport index we're using now
flat GEOMETRY vec3 eyePosition;  // world-space eye position for this camera
flat GEOMETRY vec3 eyeForward;   // world-space ray direction going forward for the camera
flat GEOMETRY Camera cam;        // world-space camera for the eye
GEOMETRY vec4 eyeDeviceCoord;    // gl_Position for the fragment shader in NDC
GEOMETRY vec3 eyeDirection;      // world-space ray direction for the current pixel
#endif // GEOMETRY

#endif // INCLUDED_VIEWPORTS_H
