define(["die","canvas"], function gl(die,canvas) {
'use strict';
try { 
  return canvas.getContext("webgl") || canvas.getContext("experimental-webgl");
} catch (e) {
  die("WebGL unavailable");
}
});
