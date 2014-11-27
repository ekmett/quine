#ifndef INCLUDED_MATH_SQUARE_H
#define INCLUDED_MATH_SQUARE_H

vec4 square(vec4 x) { return x*x; }
vec3 square(vec3 x) { return x*x; }
vec2 square(vec2 x) { return x*x; }
float square(float x) { return x*x; }

ivec4 square(ivec4 x) { return x*x; }
ivec3 square(ivec3 x) { return x*x; }
ivec2 square(ivec2 x) { return x*x; }
int square(int x) { return x*x; }

uvec4 square(uvec4 x) { return x*x; }
uvec3 square(uvec3 x) { return x*x; }
uvec2 square(uvec2 x) { return x*x; }
uint square(uint x) { return x*x; }

#endif // INCLUDED_MATH_SQUARE_H
