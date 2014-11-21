const int MAX_FOVEATIONS = 3;

uniform sampler2D foveationSampler[MAX_FOVEATIONS];
uniform vec4      foveation[MAX_FOVEATIONS];
uniform int       foveations;

// read a foveated pixel
vec4 foveated(vec2 p) {
  vec4 r = texture2D(foveationSampler[0], 0.5+(p-0.5)*foveation[0].xy)*foveation[0].z;
  for (int i=1;i<MAX_FOVEATIONS;++i) {
    if (i >= foveations) break;
    vec4 n = texture2D(foveationSampler[i], 0.5+(p-0.5)*foveation[i].xy)*foveation[0].z;
    r = r*(1-n.a)+n;
  }
  return r;
}
