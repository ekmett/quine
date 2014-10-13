define(["text","gl","performance"], function shader_plugin(text, gl, performance) {

'use strict';

function shader(file_extension,shader_type) {
  return { load : function load(name,req,onload,config) {
      name = name + "." + file_extension;
      var preload = performance.now();
      var shader = gl.createShader(gl[shader_type]);
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
          console.log(name,"error:", shader, lastError)
          gl.deleteShader(shader);
          if (onload.error) onload.error(lastError);
        }
      }, config);
    }
  };
};

return shader;

});
