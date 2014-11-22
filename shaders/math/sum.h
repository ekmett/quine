#ifndef INCLUDED_MATH_SUM_H
#define INCLUDED_MATH_SUM_H

// <https://www.opengl.org/wiki/GLSL_Optimizations#Dot_products>

float sum(vec2 a) {
  const vec2 ones = vec2(1.0);
  return dot(a,ones);
}

float sum(vec3 a) {
  const vec3 ones = vec3(1.0);
  return dot(a,ones);
}

float sum(vec4 a) {
  const vec4 ones = vec4(1.0);
  return dot(a,ones);
}

int sum(ivec2 a) { return a.x + a.y; }
int sum(ivec3 a) { return a.x + a.y + a.z; }
int sum(ivec4 a) {
  ivec2 b = a.xy + a.zw;
  return b.x + b.y;
}

uint sum(uvec2 a) { return a.x + a.y; }
uint sum(uvec3 a) { return a.x + a.y + a.z; }
uint sum(uvec4 a) {
  uvec2 b = a.xy + a.zw;
  return b.x + b.y;
}

#ifdef GL_ARB_gpu_shader_fp64
double sum(dvec2 a) {
  const dvec2 ones = dvec2(1.0);
  return dot(a,ones);
}

double sum(dvec3 a) {
  const dvec3 ones = dvec3(1.0);
  return dot(a,ones);
}

double sum(dvec4 a) {
  dvec4 ones = dvec4(1.0);
  return dot(a,ones);
}
#endif // GL_ARB_gpu_shader_fp64

#endif // INCLUDED_MATH_SUM_H
