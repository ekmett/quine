#ifndef INCLUDED_DOT_H
#define INCLUDED_DOT_H

// For some reason the standard dot only works on genType and genDType,
// it should really also support genIType.

int dot(ivec4 a, ivec4 b) { return sum(a*b); }
int dot(ivec3 a, ivec3 b) { return sum(a*b); }
int dot(ivec2 a, ivec2 b) { return sum(a*b); }

uint dot(uvec4 a, uvec4 b) { return sum(a*b); }
uint dot(uvec3 a, uvec3 b) { return sum(a*b); }
uint dot(uvec2 a, uvec2 b) { return sum(a*b); }

#endif
