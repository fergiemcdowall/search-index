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
        'facets': {'places':{}},
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
      expect(this.searchResults.facets[0].value.length).toEqual(39);
      expect(this.searchResults.facets[0].key).toEqual('places')
      expect(this.searchResults.facets[0].value[0].key).toEqual('usa')
      expect(this.searchResults.facets[0].value[0].value).toEqual(546)
      expect(this.searchResults.facets[0].value[1].key).toEqual('japan')
      expect(this.searchResults.facets[0].value[1].value).toEqual(16)
      expect(this.searchResults.facets[0].value[2].key).toEqual('uk')
      expect(this.searchResults.facets[0].value[2].value).toEqual(15)
      expect(this.searchResults.facets[0].value[3].key).toEqual('brazil')
      expect(this.searchResults.facets[0].value[3].value).toEqual(9)
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
        'facets': {'places':{'sort': 'valueAsc'}}
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets[0].value[0].key).toEqual('mexico');
      expect(this.searchResults.facets[0].value[0].value).toEqual(1);
      expect(this.searchResults.facets[0].value[1].key).toEqual('algeria');
      expect(this.searchResults.facets[0].value[1].value).toEqual(1);
      expect(this.searchResults.facets[0].value[2].key).toEqual('yemen-demo-republic');
      expect(this.searchResults.facets[0].value[2].value).toEqual(1);
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
        'facets': {'places':{'sort':'valueDesc'}},
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets[0].value[0].key).toEqual('usa');
      expect(this.searchResults.facets[0].value[0].value).toEqual(524);
      expect(this.searchResults.facets[0].value[1].key).toEqual('uk');
      expect(this.searchResults.facets[0].value[1].value).toEqual(85);
      expect(this.searchResults.facets[0].value[2].key).toEqual('japan');
      expect(this.searchResults.facets[0].value[2].value).toEqual(47);
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
        'facets': {'places':{'sort': 'keyAsc'}},
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets[0].value[0].key).toEqual('algeria');
      expect(this.searchResults.facets[0].value[0].value).toEqual(1);
      expect(this.searchResults.facets[0].value[1].key).toEqual('argentina');
      expect(this.searchResults.facets[0].value[1].value).toEqual(5);
      expect(this.searchResults.facets[0].value[2].key).toEqual('australia');
      expect(this.searchResults.facets[0].value[2].value).toEqual(17);
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
        'facets': {'places':{'sort':'keyDesc'}},
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets[0].value[0].key).toEqual('zambia');
      expect(this.searchResults.facets[0].value[0].value).toEqual(1);
      expect(this.searchResults.facets[0].value[1].key).toEqual('zaire');
      expect(this.searchResults.facets[0].value[1].value).toEqual(2);
      expect(this.searchResults.facets[0].value[2].key).toEqual('yemen-demo-republic');
      expect(this.searchResults.facets[0].value[2].value).toEqual(1);
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
        'facets': {'places':{'sort':'keyDesc','limit':20}},
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets[0].value.length).toEqual(20);
    });
  });

  it('should be able to mark a facet as active', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['reuter', '1987']
        },
        'filter': {
          'places': ['zaire']
        },
        'facets':{
          'places':{
            'sort':'keyDesc',
            'facetLength' : 20
          }
        },
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
//      console.log(JSON.stringify(this.searchResults, null, 2));
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets[0].value[0].key).toEqual('zaire');
      expect(this.searchResults.facets[0].value[0].value).toEqual(2);
      expect(this.searchResults.facets[0].value[0].active).toEqual(true);
      expect(this.searchResults.facets[0].value[1].key).toEqual('thailand');
      expect(this.searchResults.facets[0].value[1].value).toEqual(1);
      expect(this.searchResults.facets[0].value[1].active).toBeUndefined();
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
        'facets': {
          'places': {
            'sort':'valueDesc',
            'limit':20
          }
        },
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets[0].value[0].key).toEqual('japan');
      expect(this.searchResults.facets[0].value[0].value).toEqual(16);
      expect(this.searchResults.facets[0].value[0].active).toEqual(true);
      expect(this.searchResults.facets[0].value[1].key).toEqual('usa');
      expect(this.searchResults.facets[0].value[1].value).toEqual(16);
      expect(this.searchResults.facets[0].value[1].active).toEqual(true);
      expect(this.searchResults.facets[0].value[2].key).toEqual('uk');
      expect(this.searchResults.facets[0].value[2].value).toEqual(4);
      expect(this.searchResults.facets[0].value[2].active).toBeUndefined();
    });
  });

//TODO: Add more examples of active facet tagging
});

