// require.js polyfill for the CustomEvent API for IE 11
define([],function () {
  if (typeof window.CustomEvent !== "undefined") {
    return window.CustomEvent;
  }

  // polyfill
  function CustomEvent ( event, params ) {
    params = params || { bubbles: false, cancelable: false, detail: undefined };
    var evt = document.createEvent( 'CustomEvent' );
    evt.initCustomEvent( event, params.bubbles, params.cancelable, params.detail );
    return evt;
   };

  CustomEvent.prototype = window.Event.prototype;

  return CustomEvent;
});
