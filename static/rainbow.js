define(["program","staged","shader!rainbow.vs","shader!rainbow.fs"],function(program,staged,vs,fs) {
  return staged(function(gl) {
    this.pos = 0;
    this.id = program.link(vs,fs,{"pos":0}).stage().id;
    this.offset = gl.getUniformLocation(this.id,"offset");
  });
});
