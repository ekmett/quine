// requirejs-friendly polyfill for performance.now. doesn't edit the global namespace
define([],function() {
  var performance = window.performance || {};

  if (!performance.now) {
    var then = (performance.timing && performance.timing.navigationStart) ? performance.timing.navigationStart : +Date.now();
    performance.now = function() {
      return +Date.now() - then;
    };
  }
  return performance;
});
