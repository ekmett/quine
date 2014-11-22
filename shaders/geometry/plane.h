#ifndef INCLUDED_GEOMETRY_PLANE_H
#define INCLUDED_GEOMETRY_PLANE_H 1

#include "geometry/box.h"
#include "geometry/sphere.h"
#include "math/projective.h"

struct Plane {
  vec4 data;
};

vec3 normal(Plane a) {
  return a.data.xyz;
}

// compute a signed distance to the plane
float distance(Plane p, vec3 q) { return dot(p.data, point(q)); }

// returns the range of signed distances for an axis aligned bounding box to the plane
//
// intersection occurs if this range contains zero.
vec2 distance(Plane a, Box b) {
  bvec3 m = greaterThan(normal(a),vec3(0.));
  return vec2(distance(a,mix(b.hi,b.lo,m)), distance(a,mix(b.lo,b.hi,m)));
}

// returns least signed distance of intersection for an axis aligned bounding box to the plane
float minDistance(Plane a, Box b) {
  return distance(a,mix(b.lo,b.hi,lessThan(normal(a),vec3(0.))));
}

// returns greatest signed distance of intersection for an axis aligned bounding box to the plane
float maxDistance(Plane a, Box b) {
  return distance(a,mix(b.lo,b.hi,greaterThan(normal(a),vec3(0.))));
}

// returns the range of signed distances for a bounding sphere to the plane
//
// intersection occurs if this range contains zero.
vec2 distance(Plane a, Sphere b) {
  float d = distance(a,position(b));
  float r = radius(b);
  return vec2(d-r,d+r);
}

// returns the least signed distance of intersection for an axis aligned bounding box to the plane
float minDistance(Plane a, Sphere b) {
  return distance(a,position(b))-radius(b);
}

// returns the greatest signed distance of intersection for an axis aligned bounding box to the plane
float maxDistance(Plane a, Sphere b) {
  return distance(a,position(b))+radius(b);
}

// project a point onto a plane
vec3 projectToPlane(Plane p, vec3 q) {
  return q - distance(p,q) * normal(p);
}

// construct a plane from a normal and a distance
Plane plane(vec3 n, float d) {
  return Plane(vec4(n,d));
}

// construct a plane from a normal and a point
Plane plane(vec3 n, vec3 p) {
  return plane(n,dot(n,p));
}

// construct a plane from 3 non-collinear points
Plane plane(vec3 a, vec3 b, vec3 c) {
  return plane(cross(b-a,c-a),a);
}

#endif // INCLUDED_GEOMETRY_PLANE_H
