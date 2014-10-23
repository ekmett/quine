varying vec4 v_pos;
varying vec4 v_tex;

// generate a single triangle that covers the screen with a null vertex buffer
void main() {
  float q = (float)(gl_VertexID / 2);
  float r = (float)(gl_VertexID % 2);
  v_pos   = vec4 (q * 4.0 - 1.0, r * 4.0 - 1.0, 0.0, 1.0)
  v_tex   = vec2 (q * 2.0, 1.0 - r * 2.0)
}
