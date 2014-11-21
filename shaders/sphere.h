#ifndef INCLUDED_SPHERE_H
#define INCLUDED_SPHERE_H

#include "quadrance.h"
#include "square.h"

struct Sphere {
  vec4 data; // xyz : position, w: radius
};

bool overlaps(Sphere a, Sphere b) {
  return quadrance(a.data.xyz-b.data.xyz) < square(a.data.w+b.data.w);
}

vec4 position(Sphere a) {
  return vec4(a.data.xyz,1.0);
}

float radius(Sphere a) {
  return a.data.w;
}

Sphere sphere(vec3 p, float r) {
  return Sphere(vec4(p,r));
}

#endif
