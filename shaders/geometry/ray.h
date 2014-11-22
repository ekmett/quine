#ifndef INCLUDED_GEOMETRY_RAY_H
#define INCLUDED_GEOMETRY_RAY_H

#include "geometry/box.h"

struct Ray {
  vec3 origin, dir, recipDir;
};

Ray ray(vec3 origin, vec3 dir) {
  return Ray(origin,dir,vec3(1.0)/dir);
}

// returns 3 components (n1,n2,d) such that
// either d = 0, in which case the two rays are parallel.
// or n1/d and n2/d are the time of the nearest intersection
// on each ray respectively.
//
// This uses a transposed version of the math in Goldman's
// Graphics Gems article, but the det(transpose(A)) = det(A)
// so it is all good.
vec3 nearest(Ray a, Ray b) {
  vec3 i = b.origin - a.origin;
  vec3 k = cross(a.dir, b.dir);
  float d = dot(k,k);
  float n1 = determinant(mat3(i, b.dir, k));
  float n2 = determinant(mat3(i, a.dir, k));
  return vec3(n1,n2,d);
}

// value of the ray at time 't'
vec3 at(Ray a, float t) {
  return a.origin + t*a.dir;
}

// Returns the period of overlap between the two as a pair of times long the ray.
//
// When the end of the interval is less than the start they don't overlap
//
// This is SIMD version of the idea in
// <http://tavianator.com/2011/05/fast-branchless-raybounding-box-intersections/>
vec2 overlappingInterval(Ray a, Box b) {
  vec3 i = (b.lo - a.origin) * a.recipDir,
       j = (b.hi - a.origin) * a.recipDir;
  vec3 l = min(i,j),
       h = max(i,j);
  return vec2(
    max(max(l.x,l.y),max(l.z,0.0)),
    min(min(h.x,h.y),h.z)
  );
}

bool overlaps(Ray a, Box b) {
  vec2 i = overlappingInterval(a,b);
  return i.x <= i.y;
}

#endif // INCLUDED_GEOMETRY_RAY_H
