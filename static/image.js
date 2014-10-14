// Custom require.js plugin for loading images - Edward Kmett 2014
//
// Usage: add a dependency on "image!foo.gif"
//
// If you need CORS support, then you can tell require.js to configure the plugin with
// { crossOrigin : "Anonymous" }
define("image",function () {
  return {
    load : function(url,req,onload,config) {
      var image = document.createElement('img');
      if (typeof onload !== 'undefined') {
        image.addEventListener('load', function (e) {
          onload(this);
        });
        image.addEventListener('error', function (e) {
          if (onload.error) onload.error(e);
        });
        if (typeof config !== 'undefined' && typeof config.crossOrigin !== 'undefined')
          image.crossOrigin = config.crossOrigin;
        image.src = url;
      }
    }
  };
});
