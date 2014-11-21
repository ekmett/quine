#ifndef INCLUDED_SPHERE_H
#define INCLUDED_SPHERE_H

#include "quadrance.h"
#include "square.h"

struct sphere {
  vec4 s; // xyz : position, w: radius
};

bool overlaps(sphere a, sphere b) {
  return quadrance(a.s.xyz-b.s.xyz) < square(a.s.w+b.s.w);
}

vec4 position(sphere a) {
  return vec4(a.s.xyz,1.0);
}

float radius(sphere a) {
  return a.s.w;
}

sphere tosphere(vec3 p, float r) {
  return sphere(vec4(p,r));
}

#endif
