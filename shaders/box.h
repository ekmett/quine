#ifndef INCLUDED_BOX_H
#define INCLUDED_BOX_H

#include "sphere.h"

struct Box {
  vec3 lo, hi;
};

bool overlaps(Box a, Box b) {
  return all(lessThanEqual(a.lo,b.hi))
      && all(lessThanEqual(b.lo,b.hi));
}

Box bound(Box a, Box b) {
  return Box(min(a.lo,b.lo),max(a.hi,b.hi));
}

Box bound(Box a, vec3 b) {
  return Box(min(a.lo,b),max(a.hi,b));
}

vec3 position(Box a) {
  return (a.lo+a.hi)/2;
}

Sphere sphere(Box a) {
  return sphere(0.5*(a.lo+a.hi),0.5*length(a.hi-a.lo));
}

Box box(Sphere a) {
  vec3 d = vec3(a.data.w);
  return Box(a.data.xyz-d,a.data.xyz+d);
}

vec3[8] vertices(Box a) {
  return vec3[8](
     vec3(a.lo.x,a.lo.y,a.lo.z),
     vec3(a.lo.x,a.lo.y,a.hi.z),
     vec3(a.lo.x,a.hi.y,a.lo.z),
     vec3(a.lo.x,a.hi.y,a.hi.z),
     vec3(a.hi.x,a.lo.y,a.lo.z),
     vec3(a.hi.x,a.lo.y,a.hi.z),
     vec3(a.hi.x,a.hi.y,a.lo.z),
     vec3(a.hi.x,a.hi.y,a.hi.z)
  );
}

#endif
