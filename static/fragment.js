define(["text","gl"], function fragment_plugin(text, gl) {

'use strict';

return {
  load : function load(name,req,onload,config) {
    console.log("loading fragment shader", name);
    var shader = gl.createShader(gl.FRAGMENT_SHADER);
    text.load(name + ".frag",req,function(source) {
      console.log("compiling fragment shader",name)
      gl.shaderSource(shader, source);
      gl.compileShader(shader);
      if(gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        onload(shader);
      } else {
        var lastError = gl.getShaderInfoLog(shader);
        console.log("bad fragment shader", shader, lastError)
        gl.deleteShader(shader);
        if (onload.error) onload.error(lastError);
      }
    }, config);
  }
};

});
