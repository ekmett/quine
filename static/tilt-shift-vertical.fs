uniform sampler2D diffuse;
uniform float v;
uniform float r;

varying vec2 vuv;

void main() {
  vec4 sum = vec4( 0.0 );

  float vv = v * abs( r - vuv.y );

  sum += texture2D( diffuse, vec2( vuv.x, vuv.y - 4.0 * vv ) ) * 0.051;
  sum += texture2D( diffuse, vec2( vuv.x, vuv.y - 3.0 * vv ) ) * 0.0918;
  sum += texture2D( diffuse, vec2( vuv.x, vuv.y - 2.0 * vv ) ) * 0.12245;
  sum += texture2D( diffuse, vec2( vuv.x, vuv.y - 1.0 * vv ) ) * 0.1531;
  sum += texture2D( diffuse, vec2( vuv.x, vuv.y ) ) * 0.1633;
  sum += texture2D( diffuse, vec2( vuv.x, vuv.y + 1.0 * vv ) ) * 0.1531;
  sum += texture2D( diffuse, vec2( vuv.x, vuv.y + 2.0 * vv ) ) * 0.12245;
  sum += texture2D( diffuse, vec2( vuv.x, vuv.y + 3.0 * vv ) ) * 0.0918;
  sum += texture2D( diffuse, vec2( vuv.x, vuv.y + 4.0 * vv ) ) * 0.051;

  gl_FragColor = sum;
}
