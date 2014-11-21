#ifndef INCLUDED_FRUSTUM_H
#define INCLUDED_FRUSTUM_H

struct frustum {
  vec4 planes[6];
  vec4 points[8];
};

#endif
