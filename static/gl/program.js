define(["die","vs","fs","staged"], function program_plugin(die, vertex, fragment, staged) {

'use strict';

var program = {};

// attribs is an object containing {name:location} pairs
program.link = function link(v,f,attribs) {
  return staged(function(gl) {
    v.stage();
    f.stage();
    var p = this.program = gl.createProgram();
    gl.attachShader(p,v.shader);
    gl.attachShader(p,f.shader);
    if (typeof attribs !== "undefined") {
      for (var key in attribs)
        gl.bindAttribLocation(p,attribs[key],key);
    }
    gl.linkProgram(p)
    var linked = gl.getProgramParameter(p, gl.LINK_STATUS);
    if (!linked && !gl.isContextLost) {
      var lastError = gl.getProgramInfoLog(p);
      gl.deleteProgram(p);
      die("link error:" + lastError);
    }
  });
};

program.load = function load(name,req,onload,config) {
  var vf = name.split(",");
  if (vf.length < 1 || vf.length > 2) {
    onload.error("malformed program:",name);
  } else {
    var vname = vf[0];
    var fname = vf[vf.length == 1 ? 0 : 1];
    var tick = 2;
    var vs = null;
    var fs = null;
    var go = function () { onload(program.link(vs,fs)); };
    vertex.load(vname,req,function(v) { vs = v; if (! --tick) go(); }, config);
    fragment.load(fname,req,function(f) { fs = f; if (! --tick) go(); }, config);
  }
};

return program;

});
