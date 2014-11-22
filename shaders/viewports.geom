#version 410
#extension GL_ARB_shading_language_include : require

// (c) 2014 Edward Kmett

// Generate an effect pass per camera.

// This consumes a single point and generates a full-viewport triangle per
// camera. Use this for foveation or for split-screen.

#include "stage/geometry.h"
#include "viewports.h"

// TODO: invocations lock us to GL 4.0, have vertex shader send us up to MAX_VIEWPORTS points instead?
#if MAX_VIEWPORTS != 16
#error viewport count must match invocations
#endif
layout (points, invocations = 16) in; 
layout (triangle_strip, max_vertices = 3) out;

void main() {
  if (gl_InvocationID <= viewportCount) {
    for (int i=0;i<3;++i) {
      // Rendering worlds with two^H^H^Hone triangle
      float x = float((i & 1) << 2) - 1.0;
      float y = float((i & 2) << 1) - 1.0;
      gl_Position = vec4(x,y,0.0,1.0);
      gl_ViewportIndex = gl_InvocationID;
      viewportIndex    = gl_InvocationID; // TODO: gl_ViewportIndex is available in the fragment shader in GL 4.3+
      EmitVertex();
    }
    EndPrimitive();
  }
}
