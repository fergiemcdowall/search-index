var fs = require('fs');
var logger = require('../../lib/logger.js');
var si = require('../../lib/search-index.js')({ indexPath: 'si3' });


describe('indexing and search without ids', function () {

  var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/noIDs/reuters-000.json'));

  it('should index one file of test data that doesnt contain IDs', function () {
    runs(function() {
      this.err = undefined;
      var that = this;
      console.log('hello');
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
      this.msg = undefined;
      var that = this;
      si.tellMeAboutMySearchIndex(function(msg) {
        return that.msg = msg;
      });
    });
    waitsFor(function() {
      return this.msg != undefined;
    }, 'TF~*~1987~~ should have a value of 1000 in TF index ', 30000)
    runs(function () {
      expect(this.msg.totalDocs).toEqual(1000);
    });
  });

  it('should be able to search in indexed data', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['usa']
        }
      }, function(err, searchResults) {
        logger.debug(searchResults);
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toBeGreaterThan(1);
      expect(this.searchResults.hits.length).toEqual(4);
    });
  });

  it('should be able to handle multi word searches', function () {
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['reuter', '1987']
        }
      }, function(err, searchResults) {
        logger.debug(searchResults);
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toBe(100);
      expect(this.searchResults.totalHits).toBe(922);
    });
  });


});

