define(["text","staged","die"], function shader_plugin(text, staged, die) {

'use strict';

var extensions = {
  'vs' : 'VERTEX_SHADER',
  'fs' : 'FRAGMENT_SHADER'
};

return { 
  load : function load(name,req,onload,config) {
    var extension = name.split(/\./).pop();
    var type = extensions[extension];
    if (typeof type === 'undefined') die("shader: unknown file extension: " + type)
    text.load(name,req,function(source) {
      var result = staged(function(gl) {
        var s = this.id = gl.createShader(gl[type]);
        gl.shaderSource(s, source);
        gl.compileShader(s);
        if(!gl.getShaderParameter(s, gl.COMPILE_STATUS) && !gl.isContextLost()) {
          var lastError = gl.getShaderInfoLog(s);
          console.log(name,"compilation error:", s, lastError)
          gl.deleteShader(s);
        }
      });
      result.type = type;
      onload(result);
    }, config);
  }
};

});
