#ifndef INCLUDED_MATH_DOT_H
#define INCLUDED_MATH_DOT_H

#include "math/sum.h"

// For some reason the standard dot only works on genType and genDType,
// it should really also support genIType.

int idot(ivec4 a, ivec4 b) { return sum(a*b); }
int idot(ivec3 a, ivec3 b) { return sum(a*b); }
int idot(ivec2 a, ivec2 b) { return sum(a*b); }

uint udot(uvec4 a, uvec4 b) { return sum(a*b); }
uint udot(uvec3 a, uvec3 b) { return sum(a*b); }
uint udot(uvec2 a, uvec2 b) { return sum(a*b); }

#endif // INCLUDED_MATH_DOT_H
