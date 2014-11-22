#ifndef INCLUDED_VIEWPORTS_H
#define INCLUDED_VIEWPORTS_H 1

// (c) 2014 Edward Kmett

#include "camera.h"

// the maximum number of viewports we'll use
const int MAX_VIEWPORTS = 16;

//----------------------------------------------------------------------------
// Uniforms
//----------------------------------------------------------------------------

uniform int viewportCount; // number of actual viewports to use
uniform Camera cameras[MAX_VIEWPORTS]; // the set of cameras

// --------------------------------------------------------------------------------
// Geometry -> Fragment
// --------------------------------------------------------------------------------

#ifdef GEOMETRY
flat GEOMETRY int viewportIndex; // the viewport index we're using now
#endif // GEOMETRY

#endif // INCLUDED_VIEWPORTS_H
