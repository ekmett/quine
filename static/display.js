define(
  ["context", "physics", "stats", "rainbow","dds!Mountains", "staged"],
  function display(context, physics, stats, rainbow, mountains, staged) {

'use strict';

var requestId = null;

var display = staged(function (gl) { 
  rainbow.stage()
  gl.useProgram(rainbow.program);
  gl.enableVertexAttribArray(rainbow.pos);
  var verts = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, verts);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 1, -1, -1, 1, 1, 1]), gl.STATIC_DRAW);
  gl.vertexAttribPointer(rainbow.pos, 2, gl.FLOAT, false, 0, 0)
  this.gl = gl;
});

display.stats = stats.display;
  
var render = display.render = function render(t) {
  display.stage();
  var gl = context.gl;
  if (gl.isContextLost()) return; // and let the animationFrame die

  requestId = requestAnimationFrame(render);

  stats.display.begin();

  gl.clearColor(0.0, 0.0, 0.0, 1.0);                      // Set clear color to black, fully opaque
  gl.enable(gl.DEPTH_TEST);                               // Enable depth testing
  gl.depthFunc(gl.LEQUAL);                                // Near things obscure far things
  gl.clear(gl.COLOR_BUFFER_BIT|gl.DEPTH_BUFFER_BIT);      // Clear the color as well as the depth buffer.

  gl.uniform2f(rainbow.offset, 0.5, 1);
  gl.drawArrays(gl.TRIANGLE_STRIP,0,4)

  stats.display.end();
};

render(performance.now());

document.addEventListener("context-lost", function (e) { 
  console.log("WebGL context lost, epoch",e.detail.epoch);
  if (requestId != null) cancelAnimationFrame(requestId);
  requestId = null;
});

var lc = context.gl.getExtension("WEBGL_lose_context"); // grab initial context

document.addEventListener("context-restored", function (e) { 
  console.log("WebGL context restored, epoch",e.detail.epoch);
  render(performance.now()); // this will restart the rendering pipeline.
  lc = e.detail.gl.getExtension("WEBGL_lose_context"); // reset store context
});

// lose the context when I press click the mouse.
window.addEventListener("mousedown", function() {
  lc.loseContext();
})

window.addEventListener("mouseup", function() {
  lc.restoreContext();
})

return display;

});
