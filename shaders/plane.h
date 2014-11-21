#ifndef INCLUDED_PLANE_H
#define INCLUDED_PLANE_H 1

struct Plane {
  vec4 p;
};

vec3 normal(Plane a) {
  return a.p.xyz;
}

// compute a signed distance to the plane
float distance(Plane p, vec3 q) { return dot(p.p, vec4(q,1.0)); }

// project a point onto a plane
vec3 projectToPlane(Plane p, vec3 q) {
  return q - distance(p,q) * normal(p);
}

// construct a plane from 3 non-collinear points
Plane plane(vec3 a, vec3 b, vec3 c) {
  vec3 n = cross(b-a,c-a);
  return Plane(vec4(n,dot(n,a)));
}

#endif
