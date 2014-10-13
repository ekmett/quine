#ifdef GL_FRAGMENT_PRECISION_HIGH
precision highp float;
precision highp int;
#else
precision mediump float;
precision mediump int;
#endif

attribute vec2 pos;
varying vec2 coord;
uniform vec2 offset;

void main() {
  coord = pos + offset;
  gl_Position = vec4(pos, 0, 1);
}
