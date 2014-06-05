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
        }
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
      expect(this.searchResults.hits[0].id).toEqual('113');
      expect(this.searchResults.hits[1].id).toEqual('747');
      expect(this.searchResults.hits[2].id).toEqual('510');
      expect(this.searchResults.hits[3].id).toEqual('287');
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
      expect(this.searchResults.hits[0].id).toEqual('113');
      expect(this.searchResults.hits[1].id).toEqual('747');
      expect(this.searchResults.hits[2].id).toEqual('510');
      expect(this.searchResults.hits[3].id).toEqual('287');
    });
  });


  it('should be able to offset', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['japan']
        },
        'offset': 5
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
      expect(this.searchResults.hits.length).toEqual(43);
      expect(this.searchResults.hits[0].id).toEqual('272');
    });
  });


  it('should be able to set page size (limit results)', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['japan']
        },
        'pageSize': 5
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
      expect(this.searchResults.hits.length).toEqual(5);
    });
  });


  it('should be able to page (set offset and page size)', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['japan']
        },
        'offset': 5,
        'pageSize': 5
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
      expect(this.searchResults.hits.length).toEqual(5);
      expect(this.searchResults.hits[0].id).toEqual('272');
    });
  });


  it('should be able to search in indexed data with faceting', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['usa']
        },
        'facets': ['places'],
      }, function(searchResults) {
//        console.log(JSON.stringify(searchResults));
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
      expect(this.searchResults.hits[0].id).toEqual('113');
      expect(this.searchResults.hits[1].id).toEqual('747');
      expect(this.searchResults.hits[2].id).toEqual('510');
      expect(this.searchResults.hits[3].id).toEqual('287');
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
        }
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
      expect(this.searchResults.hits[0].id).toEqual('287');
    });
  });


  it('should be able to weight search results', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['usa']
        },
        'weight': {
          'body': '20'
        }
      }, function(searchResults) {
//        console.log(JSON.stringify(searchResults));
        console.log(searchResults);
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults.hits[0].id).toEqual('747');
      expect(this.searchResults.hits[1].id).toEqual('510');
      expect(this.searchResults.hits[2].id).toEqual('287');
      expect(this.searchResults.hits[3].id).toEqual('113');
    });
  });


  it('should be able to generate teasers', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['usa']
        },
        'teaser': 'title'
      }, function(searchResults) {
        console.log(searchResults);
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(JSON.stringify(this.searchResults.hits[0].document.teaser)).toEqual('"LIBERTY ALL-STAR <<span class=\\"sc-em\\">usa</span>> SETS INITIAL PAYOUT"');
    });
  });


  it('should be able to display information about the index', function () {    
    runs(function () {
      this.indexDataResponse = '';
      var that = this;
      si.indexData(function(indexDataResponse) {
        that.indexDataResponse = indexDataResponse;
      });
    });
    waitsFor(function() {
      return this.indexDataResponse != '';
    }, 'waiting for indexData response', 5000)
    runs(function() {
      expect(this.indexDataResponse.totalDocs).toEqual(1000);
    });
  });


  it('should be able to delete documents from index', function () {    
    runs(function () {
      this.indexDataResponse = '';
      var that = this;
      si.indexData(function(indexDataResponse) {
        that.indexDataResponse = indexDataResponse;
      });
    });
    waitsFor(function() {
      return this.indexDataResponse != '';
    }, 'waiting for indexData response', 5000)
    runs(function() {
      expect(true).toEqual(true);
    });
  });


  it('should be able to get documents from index', function () {    
    runs(function () {
      this.res = '';
      var that = this;
      si.getDoc(747, function(res) {
        that.res = res;
      });
    });
    waitsFor(function() {
      return this.res != '';
    }, 'waiting for response', 5000)
    runs(function() {
      expect(this.res[0].key).toEqual('DELETE-DOCUMENT~747~*');
      expect(this.res[1].key).toEqual('DELETE-DOCUMENT~747~body');
      expect(this.res[2].key).toEqual('DELETE-DOCUMENT~747~date');
      expect(this.res[3].key).toEqual('DELETE-DOCUMENT~747~places');
      expect(this.res[4].key).toEqual('DELETE-DOCUMENT~747~title');
      expect(this.res[5]['VECTOR~*~747~']).toBeDefined();
      expect(this.res[5]['VECTOR~body~747~']).toBeDefined();
      expect(this.res[5]['VECTOR~date~747~']).toBeDefined();
      expect(this.res[5]['VECTOR~places~747~']).toBeDefined();
      expect(this.res[5]['VECTOR~title~747~']).toBeDefined();
      expect(this.res[5]['VECTOR~*fielded~747~']).toBeDefined();
      expect(this.res[5]['DOCUMENT~747~']).toBeDefined();
    });
  });


});

