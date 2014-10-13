define(["gl","vertex","fragment"], function program_plugin(gl, vertex, fragment) {

'use strict';

var program = {};

// attribs is an object containing {name:location} pairs
program.link = function link(v,f,attribs) {
  var p = gl.createProgram();
  gl.attachShader(p,v);
  gl.attachShader(p,f);
  if (typeof attribs !== "undefined") {
    for (var key in attribs)
      gl.bindAttribLocation(p,attribs[key],key);
  }
  gl.linkProgram(p)
  var linked = gl.getProgramParameter(p, gl.LINK_STATUS);
  if (!linked) {
    var lastError = gl.getProgramInfoLog(p);
    gl.deleteProgram(p);
    throw lastError;
  }
  return p;
};

program.load = function load(name,req,onload,config) {
  var vf = name.split(",");
  if (vf.length != 2) {
    onload.error("malformed program!");
  } else {
    var vname = vf[0];
    var fname = vf[1];
    var tick = 2;
    var vs = null;
    var fs = null;

    var go = function () { 
      console.log("linking vertex shader",vname, "with fragment shader", fname);
      var result = null;
      var ok = false;
      try { 
        result = program.link(v,f);
        ok = true;
      } catch (e) {
        if (onload.error) onload.error(e);
        else throw e;
      }
      if (ok) onload(result);
    };
    vertex.load(vname,req,function(v) { vs = v; if (! --tick) go(); }, config);
    fragment.load(fname,req,function(f) { fs = f; if (! --tick) go(); }, config);
  }
};

return program;

});
