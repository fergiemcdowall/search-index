var fs = require('fs');
var logger = require('../../lib/logger.js');
var si = require('../../lib/search-index.js');


describe('indexing and search', function () {

  var data = JSON.parse(fs.readFileSync('test/testdata/reuters-000.json'));

  it('should index one file of test data', function () {
    runs(function() {
      this.indexingMsg = '';
      var that = this;
      si.add(data, 'reuters-000.json', ['places'], function(indexingMsg) {
        that.indexingMsg = indexingMsg;  
      });  
    });
    waitsFor(function() {
      return this.indexingMsg != '';
    }, 'indexingMsg not to be empty (search results returned)', 100000)
    runs(function () {
      expect(this.indexingMsg).toEqual('[success] indexed batch: reuters-000.json');
    });
  });


  it('verifies calibration after batch is indexed', function () {
    runs(function() {
      this.value = '';
      var that = this;
      si.indexValue('TF~*~1987~~', function(value) {
        that.value = value;
      });
    });
    waitsFor(function() {
      return this.value != '';
    }, 'TF~*~1987~~ should have a value of 1000 in TF index ', 100000)
    runs(function () {
      expect(this.value.length).toEqual(1000);
    });
  });

});

