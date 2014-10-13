define(["canvas"], function gl(canvas) {
'use strict';
try { 
  return canvas.getContext("webgl") || canvas.getContext("experimental-webgl");
} catch (e) {
  alert("WebGL unavailable");
  throw "WebGL unavailable";
}
});
