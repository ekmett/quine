#version 410

// (c) 2014 Edward Kmett

#extension GL_ARB_shading_language_include : require

#include "geometry/sphere.h"
#include "geometry/beam.h"
#include "stage/fragment.h"
#include "viewports.h"

uniform float time;

out vec4 fragColor;

const int max_iterations = 100;
const float absolute = 0.005; // 5mm absolute accuracy
const float relative = 0.5;   // half-pixel relative accuracy

// the sphere represents the point in space and the current radius of a pixel in world space.
//
// Use the radius like an fwidth to filter noise.
float de(Sphere s) {
  return length(position(s))-0.4; // for now, just put a 0.1m sphere at the origin
}

// beam trace
float march(Beam r, float t_min, float t_max, out vec4 color) {
  float t = t_min;
  color = vec4(0.0,0.0,1.0,1.0); // blue = max iterations exceeded
  for (int i=0;i<max_iterations;++i) {
    Sphere s = at(r,t); // determine the sphere of confusion
    float dt = de(s);
    if (t > t_max) {
      color = vec4(1.0,0.0,0.0,1.0); // red = timeout
      break;
    }
    if (dt < 0.00001) { // max(absolute,relative*abs(radius(s)))) {
      color = vec4(0.0,1.0,0.0,1.0); // green = hit
      break;
    }
    t += dt;
  }
  return t;
}

void main() {
  Beam b = beam(eyePosition.xyz, eyeDirection);

  // camera 
  Camera cam = viewportCamera[viewportIndex];

  // march the beam through the scene
  float t = march(b, cam.near, cam.far, fragColor); 

  // extract the z depth of our hit
  float z = -t * dot(eyeDirection, eyeForward);

  // convert to normalized device coordinates
  float ndcz = (cam.far + cam.near + (2.0*cam.far*cam.near)/z)
             / (cam.far - cam.near);

  // scribble something into the color buffer

  // map onto gl_DepthRange
  gl_FragDepth = 0.5 * (gl_DepthRange.diff * ndcz + gl_DepthRange.near + gl_DepthRange.far);
}
