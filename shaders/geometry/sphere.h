#ifndef INCLUDED_GEOMETRY_SPHERE_H
#define INCLUDED_GEOMETRY_SPHERE_H

#include "math/quadrance.h"
#include "math/square.h"

struct Sphere {
  vec4 data; // xyz : position, w: radius
};

bool overlaps(Sphere a, Sphere b) {
  return quadrance(a.data.xyz-b.data.xyz) < square(a.data.w+b.data.w);
}

vec3 position(Sphere a) {
  return a.data.xyz;
}

float radius(Sphere a) {
  return a.data.w;
}

Sphere sphere(vec3 p, float r) {
  return Sphere(vec4(p,r));
}

#endif // INCLUDED_GEOMETRY_SPHERE_H
