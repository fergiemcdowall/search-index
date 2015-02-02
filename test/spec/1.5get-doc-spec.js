var fs = require('fs');
var si = require('../../')({logSilent: false});
//var should = require('should');

describe('get-ting', function () {
  it('should be able to retreive a document by its id', function () {    
    runs(function () {
      this.result = '';
      var that = this;
      si.get(9, function(err, result) {
        that.result = result;
      });
    });
    waitsFor(function() {
      return this.result != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.result).toBeDefined();
      expect(JSON.parse(this.result).id).toEqual('9');
      expect(JSON.parse(this.result).title).toEqual('CHAMPION PRODUCTS <CH> APPROVES STOCK SPLIT');
      expect(JSON.parse(this.result).date).toEqual('26-FEB-1987 15:17:11.20');
    });
  });


  it('should be able to gracefully handle bad ID', function () {    
    runs(function () {
      this.result = '';
      this.err = '';
      var that = this;
      si.get(92827382, function(err, result) {
        that.err = err;
      });
    });
    waitsFor(function() {
      return this.err != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.err.toString()).toEqual("NotFoundError: Key not found in database [DOCUMENT~92827382~]");
    });
  });



});
