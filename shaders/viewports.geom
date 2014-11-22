#version 410
#extension GL_ARB_shading_language_include : require

// (c) 2014 Edward Kmett

// Generate an effect pass per camera.

// This consumes a single point and generates a full-viewport triangle per
// camera. Use this for foveation or for split-screen.

#include "stage/geometry.h"
#include "viewports.h"

// TODO: invocations lock us to GL 4.0, have vertex shader send us up to MAX_VIEWPORTS points instead?
layout (points, invocations = MAX_VIEWPORTS) in; 
layout (triangle_strip, max_vertices = 3) out;

void main() {
  if (gl_InvocationID <= viewportCount) {
    for (int i=0;i<3;++i) {
      // Rendering worlds with two^H^H^Hone triangle (ok, one per viewport)
      float x = float((i & 1) << 2) - 1.0;
      float y = float((i & 2) << 1) - 1.0;
      gl_Position = eyeDeviceCoord = vec4(x,y,0.0,1.0);

      Camera cam = viewportCamera[gl_InvocationID];
      float h = tan(cam.fovy/2);
      mat3 v3 = mat3(cam.modelView);
      eyePosition  = -(cam.modelView[3].xyz)*v3;
      eyeDirection = vec3(x*h*cam.aspectRatio,y*h,-1.0)*v3;

      // gl_ViewportIndex is only available in the fragment shader as of GL 4.3+
      gl_ViewportIndex = viewportIndex = gl_InvocationID;
      EmitVertex();
    }
    EndPrimitive();
  }
}
