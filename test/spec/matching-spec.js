var fs = require('fs');
var si = require('../../lib/search-index.js');

describe('generation and operation of matcher', function () {

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
      console.log(this.matchingMsg);
      expect(this.matchingMsg).toEqual(['long','london','longer','longrange','longstanding','longtime']);
    });
  });


})
