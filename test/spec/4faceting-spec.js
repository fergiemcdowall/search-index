var fs = require('fs');
var logger = require('../../lib/logger.js');
var si = require('../../lib/search-index.js');


describe('facetting', function () {

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
      expect(this.searchResults.hits.length).toEqual(4);
      expect(this.searchResults.hits[0].id).toEqual('113');
      expect(this.searchResults.hits[1].id).toEqual('747');
      expect(this.searchResults.hits[2].id).toEqual('510');
      expect(this.searchResults.hits[3].id).toEqual('287');
      expect(JSON.stringify(this.searchResults.facets))
        .toEqual(JSON.stringify({'places':[{'key':'japan','value':1},{'key':'usa','value':5}]}));
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
      expect(this.searchResults.facets.places[0].value).toEqual(525);
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



});

