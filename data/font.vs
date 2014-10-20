attribute vec2 a_uv;
attribute vec2 a_coord;

uniform mat4 u_matrix;
uniform vec2 u_size;

varying vec2 v_uv;

void main() {
  gl_Position = u_matrix * vec4(v_uv.xy, 0, 1)
  v_uv = a_uv / u_size;
}
