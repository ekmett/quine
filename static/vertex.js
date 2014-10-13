define(["text","gl","performance"], function vertex_plugin(text, gl, performance) {

'use strict';

return {
  load : function load(name,req,onload,config) {
    name = name + ".vert";
    var preload = performance.now();
    var shader = gl.createShader(gl.VERTEX_SHADER);
    text.load(name,req,function(source) {
      var postload = performance.now();
      gl.shaderSource(shader, source);
      gl.compileShader(shader);
      if(gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.log(name, "compiled",
          { load_ms: ~~(postload-preload)
          , compile_ms: ~~(performance.now()-postload)
          });
        onload(shader);
      } else {
        var lastError = gl.getShaderInfoLog(shader);
        console.log(name, "bad vertex shader:", shader, lastError)
        gl.deleteShader(shader);
        if (onload.error) onload.error(lastError);
      }
    }, config);
  }
};

});
