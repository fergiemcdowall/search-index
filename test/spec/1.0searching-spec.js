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
      expect(this.searchResults.hits.length).toEqual(100);
      expect(this.searchResults.hits[3].id).toEqual('417');
      expect(this.searchResults.hits[10].id).toEqual('972');
      expect(this.searchResults.hits[13].id).toEqual('31');
      expect(this.searchResults.hits[14].id).toEqual('171');
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

  it('should be able to return all results by doing a wildcard (*) search', function () {
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['*']
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
      expect(this.searchResults.totalHits).toBe(1000);
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
      expect(this.searchResults.hits.length).toEqual(51);
      expect(this.searchResults.hits[0].id).toEqual('271');
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
      expect(this.searchResults.hits[0].id).toEqual('271');
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
        'facets': {'places':{}},
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
      expect(this.searchResults.hits.length).toEqual(100);
      expect(this.searchResults.hits[3].id).toEqual('417');
      expect(this.searchResults.hits[10].id).toEqual('972');
      expect(this.searchResults.hits[13].id).toEqual('31');
      expect(this.searchResults.hits[14].id).toEqual('171');
      expect(this.searchResults.facets[0].value.length).toEqual(39);
      expect(this.searchResults.facets[0].key).toEqual('places')
      expect(this.searchResults.facets[0].value[0].key).toEqual('usa')
      expect(this.searchResults.facets[0].value[0].value).toEqual(546)
      expect(this.searchResults.facets[0].value[1].key).toEqual('japan')
      expect(this.searchResults.facets[0].value[1].value).toEqual(16)
      expect(this.searchResults.facets[0].value[2].key).toEqual('uk')
      expect(this.searchResults.facets[0].value[2].value).toEqual(14)
      expect(this.searchResults.facets[0].value[3].key).toEqual('brazil')
      expect(this.searchResults.facets[0].value[3].value).toEqual(9)
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
        'facets': {'places':{}},
        'filter': {
          'places': [['japan', 'japan']]
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults.hits.length).toEqual(16);
      expect(this.searchResults.hits[0].id).toEqual('676');
    });
  });


  it('should be able to search on all fields', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['reagan']
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults.totalHits).toEqual(20);
      expect(this.searchResults.hits[0].id).toEqual('796');
      expect(this.searchResults.hits[1].id).toEqual('790');
      expect(this.searchResults.hits[2].id).toEqual('801');
      expect(this.searchResults.hits[3].id).toEqual('231');
    });
  });


  it('should be able to search on one field', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          'title': ['reagan']
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults.totalHits).toEqual(10);
      expect(this.searchResults.hits[8].id).toEqual('796');
    });
  });


  it('should be able to search on one field for two terms', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          'title': ['reagan', 'baker']
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults.totalHits).toEqual(4);
      expect(this.searchResults.hits[0].id).toEqual('386');
      expect(this.searchResults.hits[1].id).toEqual('804');
      expect(this.searchResults.hits[2].id).toEqual('796');
      expect(this.searchResults.hits[3].id).toEqual('790');
    });
  });


  it('should be able to search on on two fields for seperate terms', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          'title': ['reagan'],
          'body': ['intelligence']
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults.totalHits).toEqual(4);
      expect(this.searchResults.hits[0].id).toEqual('801');
      expect(this.searchResults.hits[1].id).toEqual('386');
      expect(this.searchResults.hits[2].id).toEqual('28');
      expect(this.searchResults.hits[3].id).toEqual('869');
    });
  });

  it('should be able to search on on two fields for multiple', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          'title': ['reagan'],
          'body': ['intelligence', 'agency', 'contra']
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults.totalHits).toEqual(2);
      expect(this.searchResults.hits[0].id).toEqual('386');
      expect(this.searchResults.hits[1].id).toEqual('869');
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
      expect(this.searchResults.hits[4].id).toEqual('869');
      expect(this.searchResults.hits[5].id).toEqual('801');
    });
  });




  it('should be able to generate teasers', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['advertising']
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
      expect(JSON.stringify(this.searchResults.hits[0].document.teaser))
        .toEqual('"GREY <span class=\\"sc-em\\">advertising</span> <GREY> FORMS NEW DIVISION"');
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

