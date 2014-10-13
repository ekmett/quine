define(
  ["jquery", "performance", "stats", "gl"],
  function physics($, performance, stats) {

'use strict';

var FPS = 60;
var MILLISECONDS_PER_FRAME = 1000 / FPS;
var BURST_RATE = 5;
var MAX_LAG = FPS * 5;

var physics = {
  running : false,
  updated : 0, 
  frame   : 0
};

function step(t) {
  physics.updated = t
  ++physics.frame;
  stats.physics.begin();
  // update the world
  stats.physics.end();
}

var timer = null;

function stepper()  {
  var burst = BURST_RATE; // only catch up a few frames at a time. otherwise controls will get wonky
  var t = performance.now();
  var delay = physics.expected - t;
  while (physics.running && delay < 0 && --burst) { // we're running, we're late, and we're willing to binge
    step(t); // run a frame
    physics.expected += MILLISECONDS_PER_FRAME;
    var t2 = performance.now();
    delay = physics.expected - t2;
    // if (physics.frame % 250 == 0)
    //  console.log("physics frame",physics.frame,"at",(t/1000).toFixed(3),"with delay",(delay/1000).toFixed(3),"took",(t2-t).toFixed(1),"ms");
    t = t2;
  }
  if (physics.running && !burst) {
     var lag = -delay / MILLISECONDS_PER_FRAME;
     console.warn("physics","lagging", lag.toFixed(1), "frames");
     if (lag > MAX_LAG) {
       // if connected, this should just reset and ask the server for a big update
       console.warn("physics clock resetting due to lag");
       physics.expected = performance.now();
     }
  }
  if (physics.running) {
    // see you next time, same bat time, same bat channel
    timer = window.setTimeout(stepper, delay);
  }
}

var start = physics.start = function start() {
  if (!physics.running) {
    physics.running = true;
    physics.expected = performance.now();
    stepper();
  }
};

var stop = physics.stop = function stop() {
  if (physics.running) {
    clearTimeout(timer);
    timer = null;
    running = false;
  }
};

$(window).ready(function() {
  start();
});

return physics;

});
