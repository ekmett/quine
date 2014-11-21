#ifndef INCLUDED_FRUSTUM_H
#define INCLUDED_FRUSTUM_H

#include "box.h"
#include "plane.h"

struct Frustum {
  Plane plane[6];
  vec3  point[8];
};

// http://www.iquilezles.org/www/articles/frustumcorrect/frustumcorrect.htm
bool overlaps(Frustum a, Box b) {
  for (int i=0;i<6;++i) {
     vec4 p = a.plane[i].data;
     if (int(dot(p,vec4(b.lo.x, b.lo.y, b.lo.z, 1.0))<0.0) +
         int(dot(p,vec4(b.lo.x, b.lo.y, b.hi.z, 1.0))<0.0) +
         int(dot(p,vec4(b.lo.x, b.hi.y, b.lo.z, 1.0))<0.0) +
         int(dot(p,vec4(b.lo.x, b.hi.y, b.hi.z, 1.0))<0.0) +
         int(dot(p,vec4(b.hi.x, b.lo.y, b.lo.z, 1.0))<0.0) +
         int(dot(p,vec4(b.hi.x, b.lo.y, b.hi.z, 1.0))<0.0) +
         int(dot(p,vec4(b.hi.x, b.hi.y, b.lo.z, 1.0))<0.0) +
         int(dot(p,vec4(b.hi.x, b.hi.y, b.hi.z, 1.0))<0.0) == 8) return false;
  }

  // check frustum outside/inside box
  ivec3 c = ivec3(0), d = ivec3(0);
  for( int i=0; i<8; ++i ) {
    vec3 p = a.point[i];
    c += ivec3(greaterThan(p,b.hi));
    d += ivec3(lessThan(p,b.lo));
  }
  return !(any(equal(c,ivec3(8))) || any(equal(d,ivec3(8))));
}

bool overlaps(Box a, Frustum b) {
  return overlaps(b,a);
}

#endif
