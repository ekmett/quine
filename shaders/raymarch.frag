#version 410

// (c) 2014 Edward Kmett

#extension GL_ARB_shading_language_include : require

#include "geometry/sphere.h"
#include "geometry/beam.h"
#include "stage/fragment.h"
#include "viewports.h"

out vec4 fragColor;

const int max_iterations = 100;
const float absolute = 0.005; // 5mm absolute accuracy
const float relative = 0.5;   // half-pixel relative accuracy

// the sphere represents the point in space and the current radius of a pixel in world space.
//
// Use the radius like an fwidth to filter noise.
float de(Sphere s) {
  return length(position(s))-1.0; // for now, just put a 1m sphere at the origin
}

// beam trace
float march(Beam r, float t_min, float t_max) {
  float t = t_min;
  for (int i=0;i<max_iterations;++i) {
    Sphere s = at(r,t); // determine the sphere of confusion
    float dt = de(s);
    if (dt < max(absolute,relative*abs(radius(s))) || t > t_max) break;
    t += dt;
  }
  return t;
}

void main() {
  // beam for this pixel, TODO: dFdX for SoC?
  Beam b = beam(ray(eyePosition.xyz, eyeDirection.xyz));

  // camera 
  Camera cam = viewportCamera[viewportIndex];

  // march the beam through the scene
  float t = march(b, cam.near, cam.far); 

  // compute the forward direction
  vec3 fwd = vec3(0.0,0.0,-1.0) * mat3(cam.modelView);

  // extract the z depth of our hit
  float z = -t * dot(eyeDirection.xyz, fwd);

  // convert to normalized device coordinates
  float ndcDepth = ((cam.far+cam.near) + (2.0*cam.far*cam.near)/z)/(cam.far-cam.near);

  // scribble something into the color buffer
  fragColor = vec4(eyeDeviceCoord.xy,z,1.0);
  gl_FragDepth =((gl_DepthRange.diff * ndcDepth) + gl_DepthRange.near + gl_DepthRange.far) / 2.0;
}
