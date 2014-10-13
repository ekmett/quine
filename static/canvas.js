define(["die","domReady!"], function canvas(die,dr) {
  'use strict';
  var canvas = document.getElementById("background");
  if (!canvas) die("missing canvas");
  return canvas;
});
