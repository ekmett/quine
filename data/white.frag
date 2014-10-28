#version 410 core
#include "locations.h"
layout(location = UNIFORM_RESOLUTION) uniform vec2 iResolution = vec2(640.,480.);
layout(location = FRAGMENT_COLOR) out vec4 color;
void main() { 
  color = vec4(gl_FragCoord.xy/iResolution,1.,1.);
}
