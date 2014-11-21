#ifndef INCLUDED_FRUSTUM_H
#define INCLUDED_FRUSTUM_H

#include "box.h"
#include "plane.h"

struct Frustum {
  Plane plane[6];
  vec3  point[8];
};

// based on <http://www.iquilezles.org/www/articles/frustumcorrect/frustumcorrect.htm> but heavily vectorized
// and switched to use n/p vertices.
bool overlaps(Frustum a, Box b) {
  // check for box outside/inside frustum
  for (int i=0;i<6;++i)
    if (maxDistance(a.plane[i],b)<0.0)
      return false;

  // check frustum outside/inside box
  ivec3 c = ivec3(0), d = ivec3(0), e = ivec3(8);
  for( int i=0; i<8; ++i ) {
    vec3 p = a.point[i];
    c += ivec3(greaterThan(p,b.hi));
    d += ivec3(lessThan(p,b.lo));
  }
  return all(notEqual(c,e)) && all(notEqual(d,e));
}

bool overlaps(Box a, Frustum b) {
  return overlaps(b,a);
}

/*
// construct a frustum from:
//
// o:      origin
// d:      view direction
// u:      up vector
// n:      near distance
// f:      far distance
// fovy:   full y-direction fov
// aspect: the aspect ratio between x and y.
vec3[8] frustum(vec3 o, vec3 d, vec3 u, float n, float f, float fovy, float aspect) {
  float t = tan (fovy*0.5)
  vec3 nc = o+n*d;
  vec3 fc = o+f*d;
  vec2 wh = vec2(t*aspect,t);
  vec2 nwh = n*wh;
  vec2 fwh = f*wh;
}
*/

#endif
