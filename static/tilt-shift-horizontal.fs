uniform sampler2D diffuse;
uniform float h;
uniform float r;
varying vec2 vuv;

void main() {
  vec4 sum = vec4( 0.0 );
  float hh = h * abs( r - vuv.y );
  sum += texture2D( diffuse, vec2( vuv.x - 4.0 * hh, vuv.y ) ) * 0.051;
  sum += texture2D( diffuse, vec2( vuv.x - 3.0 * hh, vuv.y ) ) * 0.0918;
  sum += texture2D( diffuse, vec2( vuv.x - 2.0 * hh, vuv.y ) ) * 0.12245;
  sum += texture2D( diffuse, vec2( vuv.x - 1.0 * hh, vuv.y ) ) * 0.1531;
  sum += texture2D( diffuse, vec2( vuv.x, vuv.y ) ) * 0.1633;
  sum += texture2D( diffuse, vec2( vuv.x + 1.0 * hh, vuv.y ) ) * 0.1531;
  sum += texture2D( diffuse, vec2( vuv.x + 2.0 * hh, vuv.y ) ) * 0.12245;
  sum += texture2D( diffuse, vec2( vuv.x + 3.0 * hh, vuv.y ) ) * 0.0918;
  sum += texture2D( diffuse, vec2( vuv.x + 4.0 * hh, vuv.y ) ) * 0.051;
  gl_FragColor = sum;
}
