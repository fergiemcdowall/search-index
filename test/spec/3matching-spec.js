var fs = require('fs');
var si = require('../../lib/search-index.js');

describe('matching', function () {

  it('matches stuff', function () {
    runs(function() {
      this.matchingMsg = '';
      var that = this;
      si.match('lon', function(matchingMsg) {
        that.matchingMsg = matchingMsg;  
      });
    });
    waitsFor(function() {
      return this.matchingMsg != '';
    }, 'matchingMsg not to be empty', 100000)
    runs(function () {
      console.log('boomy');
      expect(this.matchingMsg).toEqual(['long','london','longer','longrange','longstanding','longtime']);
    });
  });

  it('handles match strings that are below threshold', function () {
    runs(function() {
      this.matchingMsg = undefined;
      var that = this;
      si.match('lo', function(matchingMsg) {
        that.matchingMsg = matchingMsg;  
      });
    });
    waitsFor(function() {
      return this.matchingMsg != undefined;
    }, 'matchingMsg to be empty', 100000)
    runs(function () {
      console.log(this.matchingMsg.length);
      expect(this.matchingMsg).toEqual([]);
    });
  });


})
