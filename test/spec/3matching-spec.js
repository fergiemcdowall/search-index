var fs = require('fs');
var si = require('../../')({logLevel:false});

describe('matching', function () {

  it('matches stuff', function () {
    runs(function() {
      this.matchingMsg = '';
      var that = this;
      si.match('lon', function(err, matchingMsg) {
        that.matchingMsg = matchingMsg;  
      });
    });
    waitsFor(function() {
      return this.matchingMsg != '';
    }, 'matchingMsg not to be empty', 1000)
    runs(function () {
      expect(this.matchingMsg).toEqual(['long','london','longer','longrange','longstanding','longtime']);
    });
  });

  it('handles match strings that are below threshold', function () {
    runs(function() {
      this.matchingMsg = undefined;
      var that = this;
      si.match('lo', function(err, matchingMsg) {
        that.matchingMsg = matchingMsg;  
      });
    });
    waitsFor(function() {
      return this.matchingMsg != undefined;
    }, 'matchingMsg to be empty', 1000)
    runs(function () {
      expect(this.matchingMsg).toEqual([]);
    });
  });


})
