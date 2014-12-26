var fs = require('fs');
var si = require('../../');
var should = require('should');

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
      expect(this.result).toEqual('{"title":"CHAMPION PRODUCTS <CH> APPROVES STOCK SPLIT","body":"Champion Products Inc said its\\nboard of directors approved a two-for-one stock split of its\\ncommon shares for shareholders of record as of April 1, 1987.\\n    The company also said its board voted to recommend to\\nshareholders at the annual meeting April 23 an increase in the\\nauthorized capital stock from five mln to 25 mln shares.\\n Reuter\\n\\u0003","date":"26-FEB-1987 15:17:11.20","topics":["earn"],"places":["usa"],"id":"9","*":"CHAMPION PRODUCTS <CH> APPROVES STOCK SPLIT Champion Products Inc said its\\nboard of directors approved a two-for-one stock split of its\\ncommon shares for shareholders of record as of April 1, 1987.\\n    The company also said its board voted to recommend to\\nshareholders at the annual meeting April 23 an increase in the\\nauthorized capital stock from five mln to 25 mln shares.\\n Reuter\\n\\u0003 26-FEB-1987 15:17:11.20 9 "}');
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
