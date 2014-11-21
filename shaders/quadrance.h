#ifndef INCLUDED_QUADRANCE_H
#define INCLUDED_QUADRANCE_H

#include "dot.h"

float quadrance(vec4 a) { return dot(a,a); }
float quadrance(vec3 a) { return dot(a,a); }
float quadrance(vec2 a) { return dot(a,a); }

int quadrance(ivec4 a) { return dot(a,a); }
int quadrance(ivec3 a) { return dot(a,a); }
int quadrance(ivec2 a) { return dot(a,a); }

uint quadrance(uvec4 a) { return dot(a,a); }
uint quadrance(uvec3 a) { return dot(a,a); }
uint quadrance(uvec2 a) { return dot(a,a); }

#ifdef GL_ARB_gpu_shader_fp64
double quadrance(dvec4 a) { return dot(a,a); }
double quadrance(dvec3 a) { return dot(a,a); }
double quadrance(dvec2 a) { return dot(a,a); }
#endif

#endif
