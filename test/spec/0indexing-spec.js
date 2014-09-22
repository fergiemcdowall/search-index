var fs = require('fs');
var logger = require('../../lib/logger.js');
var si = require('../../lib/search-index.js');


describe('indexing and search', function () {

  var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/reuters-000.json'));

  it('should index one file of test data', function () {
    runs(function() {
      this.err = undefined;
      var that = this;
      si.add({'batchName': 'reuters-000.json', 'filters': ['places']}, data, function(err) {
        that.err = err;
      });
    });
    waitsFor(function() {
      return this.err != undefined;
    }, 'err not to be empty (search err returned)', 30000)
    runs(function () {
      expect(this.err).toEqual(false);
    });
  });


  it('verifies calibration after batch is indexed', function () {
    runs(function() {
      this.value = undefined;
      this.err = undefined;
      var that = this;
      si.indexValue({key:'TF~*~1987~~'}, function(err, value) {
        that.err = err;
        that.value = value;
      });
    });
    waitsFor(function() {
      return this.value != undefined;
    }, 'TF~*~1987~~ should have a value of 1000 in TF index ', 30000)
    runs(function () {
      expect(this.value.length).toEqual(1000);
    });
  });

});

