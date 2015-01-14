var fs = require('fs');
var si4 = require('../../')({ indexPath: 'si4' });


describe('indexing and search without ids', function () {

  var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));

  it('should index one file of test data that doesnt contain IDs', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      si4.add({'batchName': 'justTen', 'filters': ['places']}, data, function(err) {
        that.err = err;
        that.done = true;
      });
    });
    waitsFor(function() {
      return this.done != false;
    }, 'err not to be empty (search err returned)', 30000)
    runs(function () {
      expect(this.err).toEqual(null);
    });
  });


  it('verifies calibration of number after batch is indexed', function () {
    runs(function() {
      this.msg = undefined;
      var that = this;
      si4.tellMeAboutMySearchIndex(function(msg) {
        return that.msg = msg;
      });
    });
    waitsFor(function() {
      return this.msg != undefined;
    }, 'TF~randomNumber~2749~~ should have a value of 1000 in TF index ', 30000)
    runs(function () {
      expect(this.msg.totalDocs).toEqual(10);
    });
  });


  it('should be able to search number fields in indexed data', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si4.search({
        'query': {
          '*': [2749]
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toBeGreaterThan(0);
      expect(this.searchResults.hits.length).toEqual(1);
    });
  });

});
