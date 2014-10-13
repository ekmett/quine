requirejs.config({
  paths: {
    'fiber'     : 'nih/fiber.min',
    'gl-matrix' : 'nih/gl-matrix.min',
    'domReady'  : 'nih/domReady',
    'text'      : 'nih/text',
    'jquery'    : 'nih/jquery-2.1.1.min'
  },
  shim: {
    'bootstrap': {
      deps: ['jquery'],
      exports: '$.fn.popover'
    }
  },
  enforceDefine: true
});

define(
  ['jquery','stats','display','gl','hello'],
  function ($, stats, display, hi) {
    $(document).ready(function() {
      $('body').append(stats.domElement);
      console.log(hi);
    });
  }
);
