define(["text","gl"], function vertex_plugin(text, gl) {

'use strict';

return {
  load : function load(name,req,onload,config) {
    console.log("loading vertex shader", name);
    var shader = gl.createShader(gl.VERTEX_SHADER);
    text.load(name + ".vert",req,function(source) {
      console.log("compiling vertex shader",name)
      gl.shaderSource(shader, source);
      gl.compileShader(shader);
      if(gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        onload(shader);
      } else {
        var lastError = gl.getShaderInfoLog(shader);
        console.log("bad vertex shader", shader, lastError)
        gl.deleteShader(shader);
        if (onload.error) onload.error(lastError);
      }
    }, config);
  }
};

});
