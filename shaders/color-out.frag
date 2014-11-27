#version 410

// (c) 2014 Jan-Philip Loos
//
// Just writes color out 

in vec3 color;
layout(location = 0) out vec4 fragColor;

void main() {
    fragColor = vec4(color, 1.0);
}
