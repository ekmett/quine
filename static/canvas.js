define(["domReady!"], function define_canvas(dr) {
  'use strict';
  var canvas = document.getElementById("background");
  if (!canvas) throw "missing canvas";
  return canvas;
});
