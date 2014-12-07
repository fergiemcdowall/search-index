var fs = require('fs');
var si = require('../../lib/search-index.js');


describe('get-ting', function () {
  it('should be able to retreive a document by its id', function () {    
    runs(function () {
      this.result = '';
      var that = this;
      si.get(3, function(err, result) {
        that.result = result;
      });
    });
    waitsFor(function() {
      return this.result != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.result).toBeDefined();
      console.log(this.result);
    });
  });
});
