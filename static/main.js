requirejs.config({
  paths: {
    'fiber'       : 'nih/fiber.min',
    'gl-matrix'   : 'nih/gl-matrix.min',
    'domReady'    : 'nih/domReady',
    'text'        : 'nih/text',
    'jquery'      : 'nih/jquery-2.1.1.min',
    'shader'      : 'gl/shader',
    'vs'          : 'gl/vs',
    'fs'          : 'gl/fs',
    'program'     : 'gl/program',
    'image'       : 'image',
    'CustomEvent' : 'polyfill/CustomEvent'
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
  ['jquery','stats','display','context'],
  function ($, stats, display) {
    'use strict';
    $(document).ready(function() {
      $('body').append(stats.domElement);
    });
  }
);
