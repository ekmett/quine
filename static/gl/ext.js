define(["gl","performance","die"],function ext(gl, performance, die) {

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

var ext = {};
for (var i in features) {
  ext[features[i]] = gl.getParameter(gl[features[i]]);
}

var supports = gl.getSupportedExtensions();
for (var i in supports) {
  ext[supports[i]] = gl.getExtension(supports[i]);
}

console.log("gl/ext", ~~(performance.now() - then) + " ms", ext);

ext.load = function(name,req,onload,config) {
  var result = gl.getExtension(name) || gl.getExtension("WEBKIT_" + name) || gl.getExtension("MOZ_" + name);
  if (result) onload(result);
  else if (onload.error) onload.error("Missing WebGL extension: " + name);
  else die("Missing WebGL extension: " + name);
};

return ext;

});
