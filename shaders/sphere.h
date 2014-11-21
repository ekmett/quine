#ifndef INCLUDED_SPHERE_H
#define INCLUDED_SPHERE_H

#include "quadrance.h"
#include "square.h"

struct Sphere {
  vec4 s; // xyz : position, w: radius
};

bool overlaps(Sphere a, Sphere b) {
  return quadrance(a.s.xyz-b.s.xyz) < square(a.s.w+b.s.w);
}

vec4 position(Sphere a) {
  return vec4(a.s.xyz,1.0);
}

float radius(Sphere a) {
  return a.s.w;
}

Sphere sphere(vec3 p, float r) {
  return Sphere(vec4(p,r));
}

#endif
