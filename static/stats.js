/**
 * @author mrdoob / http://mrdoob.com/ https://github.com/mrdoob/stats.js/blob/master/src/Stats.js
 * modified to use require.js, strict, and generally fit my preferences by Edward Kmett 2014
 */

define([],function stats() {
 'use strict'

 var container = document.createElement( 'div' );
 container.id = 'stats';
 // container.addEventListener( 'mousedown', function ( event ) { event.preventDefault(); setMode( ++ mode % 3 ) }, false );
 container.style.cssText = 'width:90px;opacity:0.9';

 var updateGraph = function updateGraph( dom, value ) {
  var child = dom.appendChild( dom.firstChild );
  child.style.height = value + 'px';
  var kids = dom.childNodes;
  if (kids.length > 84) {
    dom.removeChild(kids[kids.length-1]);
  }
 };

 var Stats = function Stats(name) {
   this.startTime = performance.now();
   this.prevTime = this.startTime;
   this.fpsMin = Infinity;
   this.fpsMax = 0;
   this.msMin  = Infinity
   this.msMax  = 0;
   this.frames = 0
   this.mode   = 2;

   var self = this;

   var div = this.div = document.createElement('div');
   div.addEventListener( 'mousedown', function ( event ) { event.preventDefault(); self.setMode( ++ self.mode % 3 ) }, false );
   div.style.cssText = 'width:90px;opacity:0.9;cursor:pointer;background-color:#300';
   container.appendChild(div);

   var timerText = this.timerText = document.createElement( 'div' );
   timerText.style.cssText = 'padding: 0 0 3px 3px;color:#ccc;font-family:Helvetica,Arial,sans-serif;font-size:10px;font-weight:bold;line-height:15px;text-align:center';
   timerText.innerHTML = name;
   div.appendChild( timerText );

   var fpsDiv = this.fpsDiv = document.createElement( 'div' );
   fpsDiv.style.cssText = 'padding:0 0 3px 3px;text-align:left;background-color:#002';
   div.appendChild( fpsDiv );

   var fpsText = this.fpsText = document.createElement( 'div' );
   fpsText.style.cssText = 'color:#0ff;font-family:Helvetica,Arial,sans-serif;font-size:9px;font-weight:bold;line-height:15px';
   fpsText.innerHTML = 'fps';
   fpsDiv.appendChild( fpsText );

   var fpsGraph = this.fpsGraph = document.createElement( 'div' );
   fpsGraph.style.cssText = 'position:relative;width:84px;height:30px;background-color:#0ff';
   fpsDiv.appendChild( fpsGraph );

   while ( fpsGraph.children.length < 84 ) {
     var bar = document.createElement( 'span' );
     bar.style.cssText = 'width:1px;height:30px;float:left;background-color:#113';
     fpsGraph.appendChild( bar );
   }

   var msDiv = this.msDiv = document.createElement( 'div' );
   msDiv.style.cssText = 'padding:0 0 3px 3px;text-align:left;background-color:#020;display:block';
   div.appendChild( msDiv );

   var msText = this.msText = document.createElement( 'div' );
   msText.style.cssText = 'color:#0f0;font-family:Helvetica,Arial,sans-serif;font-size:9px;font-weight:bold;line-height:15px';
   msText.innerHTML = 'ms';
   msDiv.appendChild( msText );

   var msGraph = this.msGraph = document.createElement( 'div' );
   msGraph.style.cssText = 'position:relative;width:84px;height:30px;background-color:#0f0';
   msDiv.appendChild( msGraph );

   while ( msGraph.children.length < 84 ) {
     var bar = document.createElement( 'span' );
     bar.style.cssText = 'width:1px;height:30px;float:left;background-color:#131';
     msGraph.appendChild( bar );
   }
 }

 Stats.prototype = {
   setMode : function setMode( value ) {
     this.mode = value;
     switch ( this.mode ) {
     case 0:
       this.fpsDiv.style.display = 'block';
       this.msDiv.style.display = 'none';
       break;
     case 1:
       this.fpsDiv.style.display = 'none';
       this.msDiv.style.display = 'block';
       break;
     case 2:
       this.fpsDiv.style.display = 'block';
       this.msDiv.style.display = 'block';
       break;
     default: // hide temporarily
       this.fpsDiv.style.display = 'none';
       this.msDiv.style.display = 'none';
       break;
     }
   },
   begin : function begin() {
     this.startTime = performance.now();
   },
   end: function end() {
     var time = performance.now();

     var ms = time - this.startTime;
     this.msMin = Math.min( this.msMin, ms );
     this.msMax = Math.max( this.msMax, ms );

     updateGraph( this.msGraph, Math.min( 30, 30 - ( ms / 50 ) * 30 ) );

     this.frames ++;

     if ( time > this.prevTime + 1000 ) {
       this.msText.textContent = ms.toFixed(1) + ' ms (' + this.msMin.toFixed(1) + '-' + this.msMax.toFixed(1) + ')';
       var fps = ( this.frames * 1000 ) / ( time - this.prevTime );
       this.fpsMin = Math.min( this.fpsMin, fps );
       this.fpsMax = Math.max( this.fpsMax, fps );

       this.fpsText.textContent = fps.toFixed(1) + ' fps (' + this.fpsMin.toFixed(1) + '-' + this.fpsMax.toFixed(1) + ')';
       updateGraph( this.fpsGraph, Math.min( 30, 30 - ( fps / 60 ) * 30 ) );

       this.prevTime = time;
       this.frames = 0;
     }
     return time;
   },
   update: function update() {
     startTime = this.end();
   }
 };

 var stub = {
   begin : function begin() {},
   update : function update() {},
   end : function end() {}
 };

 return {
   domElement : container,
   Stats   : Stats,
   stub    : stub,
   // display : stub,
   display : new Stats("display"),
   // physics : stub
   physics : new Stats("physics")
 };
});
