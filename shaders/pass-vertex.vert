#version 410

// (c) 2014 Jan-Philip Loos
//
// Passes vertex position & color

layout(location = 0) in vec3 aPosition;
layout(location = 3) in vec3 aColor;

out vec3 color;

void main() {
    color = aColor;
    gl_Position = vec4(aPosition, 1.0);
}
