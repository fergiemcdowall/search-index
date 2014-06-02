var fs = require('fs');
var si = require('../../lib/search-index.js');

describe('indexing and search', function () {

  it('should index one file of test data', function () {
    runs(function() {
      this.indexingMsg = '';
      var that = this;
      var data = fs.readFileSync('test/testdata/reuters-000.json');
      si.index (data, 'reuters-000.json', ['places'], function(indexingMsg) {
        that.indexingMsg = indexingMsg;  
      });  
    });
    waitsFor(function() {
      return this.indexingMsg != '';
    }, 'indexingMsg not to be empty (search results returned)', 100000)
    runs(function() {
      this.calibrationMsg = '';
      var that = this;
      si.calibrate(function(calibrationMsg) {
        that.calibrationMsg = calibrationMsg;
      });
    });
    waitsFor(function() {
      return this.calibrationMsg != '';
    }, 'calibrationMsg not to be emtpy (index calibrated)', 100000)
    runs(function () {
      expect(this.indexingMsg).toEqual('indexed batch: reuters-000.json\n');
      expect(this.calibrationMsg).toEqual('calibrated 1000 docs');
    });
  });

  it('should be able to search in indexed data', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['usa']
        },
        'facets': ['places'],
        'offset': '0',
        'pageSize': '20'
      }, function(searchResults) {
        console.log(searchResults);
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toBeGreaterThan(1);
      expect(this.searchResults.hits.length).toEqual(4);
      expect(JSON.stringify(this.searchResults.facets))
        .toEqual(JSON.stringify({places:{usa:4,japan:1}}));
    });
  });

  it('should be able to filter search results', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['usa']
        },
        'facets': ['places'],
        'filter': {
          'places': ['japan']
        },
        'offset': '0',
        'pageSize': '20'
      }, function(searchResults) {
        console.log(searchResults);
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults.hits.length).toEqual(1);
    });
  });


});

