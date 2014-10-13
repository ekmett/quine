precision mediump float;
varying vec2 coord;
void main() {
  gl_FragColor = vec4(coord, 0, 1);
}
