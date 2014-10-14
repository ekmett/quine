define(["context"],function(context) {

'use strict';

// used to build resources that can reset after loss of context
function Staged(f,eager) {
  this.stage = function() {
    if (this.epoch < context.epoch) {
      f.call(this,context.gl);
      this.epoch = context.epoch;
    }
    return this;
  };
  if (eager) this.stage();
}

Staged.prototype = {
  epoch: 0,
  value: null
};

function staged(f,eager) { return new Staged(f,eager); }

staged.Staged = Staged;

return staged;

});
