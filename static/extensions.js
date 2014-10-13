define(["gl","performance"],function extensions(gl, performance) {

var then = performance.now();

var features = [
  "RENDERER",
  "VENDOR",
  "VERSION",
  "SHADING_LANGUAGE_VERSION",
  "RED_BITS", "GREEN_BITS", "BLUE_BITS", "ALPHA_BITS",
  "DEPTH_BITS", "STENCIL_BITS",
  "ALIASED_LINE_WIDTH_RANGE",
  "MAX_COMBINED_TEXTURE_IMAGE_UNITS",
  // "NUM_COMPRESSED_TEXTURE_FORMATS",
  "MAX_CUBE_MAP_TEXTURE_SIZE",
  "MAX_FRAGMENT_UNIFORM_VECTORS",
  "MAX_RENDERBUFFER_SIZE",
  "MAX_TEXTURE_IMAGE_UNITS",
  "MAX_TEXTURE_SIZE",
  "MAX_VARYING_VECTORS",
  "MAX_VERTEX_ATTRIBS",
  "MAX_VERTEX_TEXTURE_IMAGE_UNITS",
  "MAX_VERTEX_UNIFORM_VECTORS",
  "MAX_VIEWPORT_DIMS"
];

var extensions = {};
for (var i in features) {
  extensions[features[i]] = gl.getParameter(gl[features[i]]);
}

var supports = gl.getSupportedExtensions();
for (var i in supports) {
  extensions[supports[i]] = +i;
}

console.log("extensions:", ~~(performance.now() - then) + " ms", extensions);


return extensions;

});
