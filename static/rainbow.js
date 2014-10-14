define(["program","staged","vs!rainbow","fs!rainbow"],function(program,staged,vertex,fragment) {
  return staged(function(gl) {
    this.pos = 0;
    this.program = program.link(vertex,fragment,{"pos":0}).stage().program;
    this.offset = gl.getUniformLocation(this.program,"offset");
  });
});
