define(["gl","program","vertex!hello","fragment!hello"],function(gl,program,v,f) {
  var p = program.link(v,f,{"pos":0});
  return { 
    pos: 0, // gl.getAttribLocation(p,"pos"),
    offset: gl.getUniformLocation(p,"offset"),
    program: p
  };
});
