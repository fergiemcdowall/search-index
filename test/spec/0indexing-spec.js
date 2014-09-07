var fs = require('fs');
var logger = require('../../lib/logger.js');
var si = require('../../lib/search-index.js');


describe('indexing and search', function () {

  var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/reuters-000.json'));


  it('should index one file of test data', function () {
    runs(function() {
      this.indexingMsg = '';
      var that = this;
      console.log('also in here');
      si.add(data, 'reuters-000.json', ['places'], function(indexingMsg) {
        console.log('in here');
        that.indexingMsg = indexingMsg;  
      });  
    });
    waitsFor(function() {
      return this.indexingMsg != '';
    }, 'indexingMsg not to be empty (search results returned)', 30000)
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
      console.log(this.value);
      expect(this.value.length).toEqual(1000);
    });
  });

});

