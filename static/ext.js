define(["die","staged"],function ext(die, staged) {

'use strict';

// purely informative stats
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

var ext = staged(function(gl) { 
  for (var i in features) {
    this[features[i]] = gl.getParameter(gl[features[i]]);
  }
  // these stats tied to the current gl
  var supports = gl.getSupportedExtensions();
  for (var i in supports) {
    this[supports[i]] = gl.getExtension(supports[i]);
  }
}, true);

ext.load = function(name,req,onload,config) {
  onload(staged(function(gl) {
    var result = gl.getExtension(name) || gl.getExtension("WEBKIT_" + name) || gl.getExtension("MOZ_" + name);
    if (!result && !gl.isContextLost()) die("Missing WebGL extension: " + name);
    return result;
  }));
};

return ext;

});
