#ifndef INCLUDED_SUM_H
#define INCLUDED_SUM_H

// L1 norm

float sum(float a) { return a; }
float sum(vec2 a) { return a.x + a.y; }
float sum(vec3 a) { return a.x + a.y + a.z; }
float sum(vec4 a) { return a.x + a.y + a.z + a.w; }

int sum(int a) { return a; }
int sum(ivec2 a) { return a.x + a.y; }
int sum(ivec3 a) { return a.x + a.y + a.z; }
int sum(ivec4 a) { return a.x + a.y + a.z + a.w; }

uint sum(uint a) { return a; }
uint sum(uvec2 a) { return a.x + a.y; }
uint sum(uvec3 a) { return a.x + a.y + a.z; }
uint sum(uvec4 a) { return a.x + a.y + a.z + a.w; }

#ifdef GL_ARB_gpu_shader_fp64
double sum(double a) { return a; }
double sum(dvec2 a) { return a.x + a.y; }
double sum(dvec3 a) { return a.x + a.y + a.z; }
double sum(dvec4 a) { return a.x + a.y + a.z + a.w; }
#endif

#endif

