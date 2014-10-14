requirejs.config({
  paths: {
    // 'fiber'       : 'nih/fiber.min',
    // 'gl-matrix'   : 'nih/gl-matrix.min',
    'domReady'    : 'nih/domReady',
    'text'        : 'nih/text',
    'jquery'      : 'nih/jquery-2.1.1.min',
    'image'       : 'image',
    'CustomEvent' : 'polyfill/CustomEvent',
    'shader'      : 'gl/shader',
    'program'     : 'gl/program'
  },
  enforceDefine: true
});

define(
  ['jquery','stats','display'],
  function ($, stats, display) {
    'use strict';
    $(document).ready(function() {
      $('body').append(stats.domElement);
    });
  }
);
