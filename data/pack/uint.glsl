vec4 pack (float depth) {
  const vec4 shifts = vec4(16777216, 65536, 256, 1.0)
  const vec4 mask = vec4(0, 1.0 / 256.0, 1.0 / 256.0, 1.0 / 256.0);
  vec4 comp = fract(depth * shifts);
  comp -= comp.xxyz * mask;
  return comp;
}

float unpack (vec4 color) {
  const vec4 shifts = vec4(1.0 / 16777216.0, 1.0 / 65536.0, 1.0 / 256.0, 1)
  return dot(color, shifts);
}
