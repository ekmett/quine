define(["text","staged"], function shader_plugin(text, staged) {

'use strict';

function shader(file_extension,shader_type) {
  return { load : function load(name,req,onload,config) {
      name = name + "." + file_extension;
      text.load(name,req,function(source) {
        var result = staged(function(gl) {
          var s = this.shader = gl.createShader(gl[shader_type]);
          gl.shaderSource(s, source);
          gl.compileShader(s);
          if(!gl.getShaderParameter(s, gl.COMPILE_STATUS) && !gl.isContextLost()) {
            var lastError = gl.getShaderInfoLog(s);
            console.log(name,"compilation error:", s, lastError)
            gl.deleteShader(s);
          }
        });
        result.shader_type = shader_type;
        onload(result);
      }, config);
    }
  };
};

return shader;

});
