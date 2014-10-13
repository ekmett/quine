define([],function () {
return function die(msg) {
  alert(msg);
  throw msg;
};
});
