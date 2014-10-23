uniform sampler1D u_packed0;    // pos = packed0.rgb, age = packed0.a, sign(age) indicates at rest vs. moving, FP32
uniform sampler1D u_packed1;    // up  = packed1.xy, two 8-bit advected quantities = up.zw                   , RGBA8888
// uniform sampler1D u_packed2; // vel = packed2.xyz, delta_age = packed2.a                                  , FP16

uniform mat3 u_invView;       // inverse view transformation matrix
uniform mat4 u_worldViewProj; // world view projection matrix

varying float v_age; // age of the vertex. sign indicates restfulness
varying vec4  v_tex; // texture coords and advected quantities

// v%6,  x,  y
// 0  , -1,  1
// 1  ,  1,  1
// 2  , -1, -1
// 3  ,  1, -1

void main() {
  uint q      = gl_VertexID / 4;
  uint r      = gl_VertexID % 4;
  vec4 up     = texelFetch(u_packed1, q);
  vec4 pos    = texelFetch(u_packed0, q);
  vec2 offset = mat2(up.xy,-up.y,up.x) * vec2(r % 2 ? 1.0 : -1.0, r & 2 ? -1.0 : 1.0);
  gl_Position = u_worldViewProj * vec4(vec3(offset, 0.0) * u_invView + pos.xyz, 1.0);
  v_age       = pos.a;
  v_tex       = vec4(r % 2 ? 1.0 : 0.0, r & 2 ? 1.0 : 0.0, up.ba); // texture coordinates and advected quantities
}
