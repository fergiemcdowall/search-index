var fs = require('fs');
var si = require('../../')({logLevel:false});


describe('indexing and search', function () {


  it('should be able to search in indexed data', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['usa']
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
      expect(this.searchResults.hits.length).toBeGreaterThan(1);
      expect(this.searchResults.hits.length).toEqual(4);
      expect(this.searchResults.hits[0].id).toEqual('113');
      expect(this.searchResults.hits[1].id).toEqual('747');
      expect(this.searchResults.hits[2].id).toEqual('510');
      expect(this.searchResults.hits[3].id).toEqual('287');
    });
  });


  it('should be able to handle 0 results', function () {
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['asdijasdjasdadssadmakesnosense']
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toBe(0);
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


  it('should be able to handle multi word searches where some words are not present in index', function () {
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['reuter', 'yorkxxxxxxx']
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
      expect(this.searchResults.hits.length).toEqual(0);
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
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toBeGreaterThan(1);
      expect(this.searchResults.hits.length).toEqual(43);
      expect(this.searchResults.hits[0].id).toEqual('991');
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
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
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
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toBeGreaterThan(1);
      expect(this.searchResults.hits.length).toEqual(5);
      expect(this.searchResults.hits[0].id).toEqual('991');
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
      }, function(err, searchResults) {
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
      expect(this.searchResults.hits[0].id).toEqual('113');
      expect(this.searchResults.hits[1].id).toEqual('747');
      expect(this.searchResults.hits[2].id).toEqual('510');
      expect(this.searchResults.hits[3].id).toEqual('287');
      expect(JSON.stringify(this.searchResults.facets))
        .toEqual(JSON.stringify({'places':[{'key':'usa','value':4},{'key':'japan','value':1}]}));
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
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
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
          'body': ['reagan'],
          'title': ['reagan']
        },
        'weight': {
          'body': '20'
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults.hits[0].id).toEqual('231');
      expect(this.searchResults.hits[1].id).toEqual('804');
      expect(this.searchResults.hits[2].id).toEqual('801');
      expect(this.searchResults.hits[3].id).toEqual('869');
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
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(JSON.stringify(this.searchResults.hits[0].document.teaser)).toEqual('"LIBERTY ALL-STAR <<span class=\\"sc-em\\">usa</span>> SETS INITIAL PAYOUT"');
    });
  });



  it('should be able to display information about the index', function () {    
    runs(function () {
      this.tellMeAboutMySearchIndexResponse = '';
      var that = this;
      si.tellMeAboutMySearchIndex(function(tellMeAboutMySearchIndexResponse) {
        that.tellMeAboutMySearchIndexResponse = tellMeAboutMySearchIndexResponse;
      });
    });
    waitsFor(function() {
      return this.tellMeAboutMySearchIndexResponse != '';
    }, 'waiting for indexData response', 1000)
    runs(function() {
      expect(this.tellMeAboutMySearchIndexResponse.totalDocs).toEqual(1000);
    });
  });

});

