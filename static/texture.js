define(["staged"],function texture(staged) {

// a texture has an image format

var texture = function(image) { 
  var result = staged(function(gl) {
    this.texture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, this.texture);      
    gl.texImage2D(gl.TEXTURE_2D, 0, gl[this.format],  gl.UNSIGNED_BYTE, this.image);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl[this.mag_filter]);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl[this.min_filter]);
    if (this.generateMipmaps) gl.generateMipmap(gl.TEXTURE_2D);
    gl.bindTexture(gl.TEXTURE_2D, null);
  });
  result.format = "RGBA";
  result.image = image;
  result.mag_filter = "LINEAR";
  result.min_filter = "LINEAR_MIPMAP_NEAREST";
  result.generateMipMaps = false;
};

return texture;
  
});
