define(["canvas","CustomEvent","die"], function context(canvas,CustomEvent,die) {

function getGL() {
  try {
    return canvas.getContext("webgl") || canvas.getContext("experimental-webgl");
  } catch (e) {
    die("WebGL unavailable");
  }
}

var context = { 
  gl    : getGL(),
  epoch : 1  // every time we lose and restore gl context, we raise the epoch making it easier to detect
};

// prevent this from causing the whole page to die
canvas.addEventListener("webglcontextlost", function(e) {
  document.dispatchEvent(new CustomEvent('context-lost', { epoch : context.epoch }));
  e.preventDefault();
});

canvas.addEventListener("webglcontextrestored", function(e) {
  // go fetch a new gl object
  context.epoch++;
  context.gl = getGL();
  document.dispatchEvent(new CustomEvent('context-restored', context));
});

return context;

});
