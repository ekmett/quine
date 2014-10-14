define([],function() {
  return {
    load : function(url,req,onload,config) {
      var request = new XMLHttpRequest();
      request.open('GET',url,true);
      if (onload) {
        request.addEventListener('load', function (e) { onload(this.response); });
        if (onload.error) request.addEventListener('error', onload.error);
      }
      if (config.crossOrigin !== undefined) request.crossOrigin = config.crossOrigin;
      if (config.responseType !== undefined) request.responseType = config.responseType;
      request.send(null);
    }
  };
});
