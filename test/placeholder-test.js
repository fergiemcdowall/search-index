var buster = require('buster');
var si = require('../lib/search-index/index.js');

buster.testCase("My thing", {
  "has the foo and bar": function () {
    assert.equals("foo", "foo");
  },
  
  "states the obvious": function () {
    assert(true);
  },
  
  "search-index calibraton": function (done) {
    si.calibrate(function(msg) {
      console.log(msg);
      done(assert(true));
    });
  }
})
