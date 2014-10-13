define(["text","gl","performance"], function fragment_plugin(text, gl, performance) {

'use strict';

return {
  load : function load(name,req,onload,config) {
    name = name + ".frag";
    var preload = performance.now();
    var shader = gl.createShader(gl.FRAGMENT_SHADER);
    text.load(name,req,function(source) {
      var postload = performance.now();
      gl.shaderSource(shader, source);
      gl.compileShader(shader);
      if(gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.log(name,"compiled",
          { load_ms : ~~(postload-preload),
            compile_ms : ~~(performance.now()-postload)
          });
        onload(shader);
      } else {
        var lastError = gl.getShaderInfoLog(shader);
        console.log(name,"bad fragment shader:", shader, lastError)
        gl.deleteShader(shader);
        if (onload.error) onload.error(lastError);
      }
    }, config);
  }
};

});
