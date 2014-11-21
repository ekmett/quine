#ifndef INCLUDED_BOX_H
#define INCLUDED_BOX_H

#include "sphere.h"

struct box {
  vec3 lo, hi;
};

bool overlaps(box a, box b) {
  return all(lessThanEqual(a.lo,b.hi))
      && all(lessThanEqual(b.lo,b.hi));
}

box bound(box a, box b) {
  return box(min(a.lo,b.lo),max(a.hi,b.hi));
}

box bound(box a, vec3 b) {
  return box(min(a.lo,b),max(a.hi,b));
}

vec3 position(box a) {
  return (a.lo+a.hi)/2;
}

sphere tosphere(box a) {
  return tosphere(0.5*(a.lo+a.hi),0.5*length(a.hi-a.lo));
}

#endif
