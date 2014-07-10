var fs = require('fs');
var si = require('../../lib/search-index.js');

describe('generation and operation of matcher', function () {


  it('generate matcher', function () {
    runs(function() {
      this.matchingMsg = '';
      var that = this;
      si.generateMatcher(function(matchingMsg) {
        that.matchingMsg = matchingMsg;  
      });
    });
    waitsFor(function() {
      return this.matchingMsg != '';
    }, 'matchingMsg not to be empty', 100000)
    runs(function () {
      expect(this.matchingMsg).toEqual('[success] finished generating matcher with 10439 tokens\n');
    });
  });

  it('matches stuff', function () {
    runs(function() {
      this.matchingMsg = '';
      var that = this;
      si.matcher('lon', function(matchingMsg) {
        that.matchingMsg = matchingMsg;  
      });
    });
    waitsFor(function() {
      return this.matchingMsg != '';
    }, 'matchingMsg not to be empty', 100000)
    runs(function () {
      console.log(this.matchingMsg);
      expect(this.matchingMsg).toEqual(['london','long','longer','longrange','longstanding','longtime']);
    });
  });


})
