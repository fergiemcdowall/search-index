var fs = require('fs');
var si = require('../../');


describe('faceting', function () {

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
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toBeGreaterThan(1);
      expect(this.searchResults.hits.length).toEqual(100);
      expect(this.searchResults.hits[3].id).toEqual('417');
      expect(this.searchResults.hits[12].id).toEqual('455');
      expect(this.searchResults.hits[13].id).toEqual('31');
      expect(this.searchResults.hits[16].id).toEqual('77');
      expect(JSON.stringify(this.searchResults.facets))
        .toEqual(JSON.stringify({"places":[{"key":"usa","value":546},{"key":"japan","value":16},{"key":"uk","value":15},{"key":"brazil","value":9},{"key":"taiwan","value":5},{"key":"china","value":4},{"key":"ussr","value":4},{"key":"australia","value":4},{"key":"west-germany","value":3},{"key":"france","value":3}]}));
    });
  });

  it('should be able to sort facets by value ascending', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['reuter']
        },
        'facetSort' : 'valueAsc',
        'facets': ['places'],
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets.places[0].key).toEqual('mexico');
      expect(this.searchResults.facets.places[0].value).toEqual(1);
      expect(this.searchResults.facets.places[1].key).toEqual('algeria');
      expect(this.searchResults.facets.places[1].value).toEqual(1);
      expect(this.searchResults.facets.places[2].key).toEqual('yemen-demo-republic');
      expect(this.searchResults.facets.places[2].value).toEqual(1);
    });
  });

  it('should be able to sort facets by value descending', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['reuter']
        },
        'facetSort' : 'valueDesc',
        'facets': ['places'],
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets.places[0].key).toEqual('usa');
      expect(this.searchResults.facets.places[0].value).toEqual(524);
      expect(this.searchResults.facets.places[1].key).toEqual('uk');
      expect(this.searchResults.facets.places[1].value).toEqual(85);
      expect(this.searchResults.facets.places[2].key).toEqual('japan');
      expect(this.searchResults.facets.places[2].value).toEqual(47);
    });
  });

  it('should be able to sort facets by key ascending', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['reuter']
        },
        'facetSort' : 'keyAsc',
        'facets': ['places'],
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets.places[0].key).toEqual('algeria');
      expect(this.searchResults.facets.places[0].value).toEqual(1);
      expect(this.searchResults.facets.places[1].key).toEqual('argentina');
      expect(this.searchResults.facets.places[1].value).toEqual(5);
      expect(this.searchResults.facets.places[2].key).toEqual('australia');
      expect(this.searchResults.facets.places[2].value).toEqual(17);
    });
  });

  it('should be able to sort facets by key descending', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['reuter']
        },
        'facetSort' : 'keyDesc',
        'facets': ['places'],
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets.places[0].key).toEqual('zambia');
      expect(this.searchResults.facets.places[0].value).toEqual(1);
      expect(this.searchResults.facets.places[1].key).toEqual('zaire');
      expect(this.searchResults.facets.places[1].value).toEqual(2);
      expect(this.searchResults.facets.places[2].key).toEqual('yemen-demo-republic');
      expect(this.searchResults.facets.places[2].value).toEqual(1);
    });
  });

  it('should be able to limit facet length', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['reuter']
        },
        'facetSort' : 'keyDesc',
        'facetLength' : 20,
        'facets': ['places'],
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets.places.length).toEqual(20);
    });
  });

  it('should be able to mark a facet as active', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['reuter']
        },
        'filter': {
          'places': ['zaire']
        },
        'facetSort' : 'keyDesc',
        'facetLength' : 20,
        'facets': ['places'],
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets.places[0].key).toEqual('zaire');
      expect(this.searchResults.facets.places[0].value).toEqual(2);
      expect(this.searchResults.facets.places[0].active).toEqual(true);
      expect(this.searchResults.facets.places[1].key).toEqual('thailand');
      expect(this.searchResults.facets.places[1].value).toEqual(1);
      expect(this.searchResults.facets.places[1].active).toBeUndefined();
    });
  });


  it('should be able to mark multiple facets as active', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['reuter']
        },
        'filter': {
          'places': ['usa', 'japan']
        },
        'facetSort' : 'valueDesc',
        'facetLength' : 20,
        'facets': ['places'],
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets.places[0].key).toEqual('japan');
      expect(this.searchResults.facets.places[0].value).toEqual(16);
      expect(this.searchResults.facets.places[0].active).toEqual(true);
      expect(this.searchResults.facets.places[1].key).toEqual('usa');
      expect(this.searchResults.facets.places[1].value).toEqual(16);
      expect(this.searchResults.facets.places[1].active).toEqual(true);
      expect(this.searchResults.facets.places[2].key).toEqual('uk');
      expect(this.searchResults.facets.places[2].value).toEqual(4);
      expect(this.searchResults.facets.places[2].active).toBeUndefined();
    });
  });

//TODO: Add more examples of active facet tagging



});

