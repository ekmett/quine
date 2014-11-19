const int FOVEATIONS = 3;

// GL_BORDER_COLOR: (0.0,0.0,0.0,0.0)
// GL_TEXTURE_SWIZZLE_R: GL_RED
// GL_TEXTURE_SWIZZLE_A: GL_ONE
uniform sampler2D foveations[FOVEATIONS];
uniform vec4      foveationOffset[FOVEATIONS];
uniform vec4      foveationScale[FOVEATIONS];
uniform int       clipmapLevels;

float clipmap(vec4 p) {
  float distance = texture3D(clipmapTexture[0], ((p-clipmapOffset[0])*clipmapScale[0]).xyz).r;
  for (int i=1;i<CLIPMAPS;++i) {
    if (i >= clipmapLevels) break;
    float new = texture3D(clipmapTexture[i], ((p-clipmapOffset[i])*clipmapScale[i]).xyz).r;
    distance = distance*(1-new.a)+new;
  }
  return distance;
}
