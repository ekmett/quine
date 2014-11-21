#version 410

#define MAX_FOVEATIONS 3

layout (points, invocations = MAX_CAMERAS) in;
layout (triangle_strip, max_vertices = 3) out;

struct camera {
  float fovy, nearz, farz;
  mat44 projection, inverseProjection;
  mat44 modelView, inverseModelView;
} cameras[MAX_CAMERAS]


uniform int foveations;
uniform int foveatedCamera[MAX_FOVEATIONS];
uniform int foveationWidth[MAX_FOVEATIONS];
uniform mat44 foveatedPerspective[MAX_FOVEATIONS];
uniform mat44 inverseFoveatedPerspective[MAX_FOVEATIONS];

out flat int foveation;
out flat vec2 current_foveation_size;
out flat float current_foveation_fovy;
