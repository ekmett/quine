#ifndef INCLUDE_PROJECTIVE_H
#define INCLUDE_PROJECTIVE_H

// convert affine point to projective point (w/ fused multiply-add)
//
// point(x) = vec4(x,1.0)
//
// https://www.opengl.org/wiki/GLSL_Optimizations#Assignment_with_MAD

vec4 point(vec3 v) {
  const vec2 zo = vec2(0.0,1.0);
  return v.xyzz * zo.yyyx + zo.xxxy;
}

// convert affine vector to projective vector (w/ fused multiply-add)
//
// point(x) = vec4(x,0.0);
//
// https://www.opengl.org/wiki/GLSL_Optimizations#Assignment_with_MAD

vec4 vector(vec3 v) {
  const vec2 zo = vec2(0.0,1.0);
  return v.xyzz * zo.yyyx + zo.xxxx;
}

#endif
