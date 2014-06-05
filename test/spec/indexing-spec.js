var fs = require('fs');
var si = require('../../lib/search-index.js');

describe('indexing and search', function () {

  var data = fs.readFileSync('test/testdata/reuters-000.json');


  it('should index one file of test data', function () {
    runs(function() {
      this.indexingMsg = '';
      var that = this;
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
      expect(this.res['DELETE-DOCUMENT~747~*']).toBeDefined();
      expect(this.res['DELETE-DOCUMENT~747~body']).toBeDefined();
      expect(this.res['DELETE-DOCUMENT~747~date']).toBeDefined();
      expect(this.res['DELETE-DOCUMENT~747~places']).toBeDefined();
      expect(this.res['DELETE-DOCUMENT~747~title']).toBeDefined();
      expect(this.res['VECTOR~*~747~']).toBeDefined();
      expect(this.res['VECTOR~body~747~']).toBeDefined();
      expect(this.res['VECTOR~date~747~']).toBeDefined();
      expect(this.res['VECTOR~places~747~']).toBeDefined();
      expect(this.res['VECTOR~title~747~']).toBeDefined();
      expect(this.res['VECTOR~*fielded~747~']).toBeDefined();
      expect(this.res['DOCUMENT~747~']).toBeDefined();
    });
  });


  it('should be able to delete documents from index', function () {    
    runs(function () {
      this.res = '';
      var that = this;
      si.deleteDoc(747, function(res) {
        that.res = res;
      });
    });
    waitsFor(function() {
      return this.res != '';
    }, 'waiting for indexData response', 5000)
    runs(function() {
      console.log(this.res);
      expect(true).toEqual(true);
    });
  });



  it('should verify delete', function () {    
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
      expect(this.res['DELETE-DOCUMENT~747~*']).toBeUndefined();
      expect(this.res['DELETE-DOCUMENT~747~body']).toBeUndefined();
      expect(this.res['DELETE-DOCUMENT~747~date']).toBeUndefined();
      expect(this.res['DELETE-DOCUMENT~747~places']).toBeUndefined();
      expect(this.res['DELETE-DOCUMENT~747~title']).toBeUndefined();
      expect(this.res['VECTOR~*~747~']).toBeUndefined();
      expect(this.res['VECTOR~body~747~']).toBeUndefined();
      expect(this.res['VECTOR~date~747~']).toBeUndefined();
      expect(this.res['VECTOR~places~747~']).toBeUndefined();
      expect(this.res['VECTOR~title~747~']).toBeUndefined();
      expect(this.res['VECTOR~*fielded~747~']).toBeUndefined();
      expect(this.res['DOCUMENT~747~']).toBeUndefined();
    });
  });

  it('deleted document is not appearing in results', function () {    
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
      expect(this.searchResults.hits.length).toEqual(3);
      expect(this.searchResults.hits[0].id).toEqual('113');
      expect(this.searchResults.hits[1].id).toEqual('510');
      expect(this.searchResults.hits[2].id).toEqual('287');
    });
  });


  it('should reindex deleted document', function () {
    runs(function() {
      this.indexingMsg = '';
      var that = this;
//      var data = fs.readFileSync('test/testdata/reuters-000.json');
      var singleDoc = {};
      singleDoc['747'] = JSON.parse(data)['747'];
      var stringifiedSingleDoc = JSON.stringify(singleDoc);
      console.log(stringifiedSingleDoc);
      si.index (stringifiedSingleDoc, 'reuters-000.json', ['places'], function(indexingMsg) {
        that.indexingMsg = indexingMsg;
      });  
    });
    waitsFor(function() {
      return this.indexingMsg != '';
    }, 'indexingMsg not to be empty (search results returned)', 100000)
    runs(function () {
      expect(this.indexingMsg).toEqual('indexed batch: reuters-000.json\n');
    });
  });


  it('document reappears in search', function () {    
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

});

