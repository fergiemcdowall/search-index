var fs = require('fs');
var si = require('../../')({indexPath: 'sifiltering', logSilent: false});


describe('indexing and search', function () {


  var data = JSON.parse(fs.readFileSync('test/datasets/twitter-tweets.json'));

  it('should index one file of test data', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      si.add({'batchName': 'twitter-tweets.json', 'filters': ['tags', 'user']}, data, function(err) {
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

  it('should be able to search in indexed data', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['search']
        },
        'facets': ['user','tags']
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets.user[0]).toBeDefined();
      expect(this.searchResults.facets.user[0].key).toEqual('eklem');
      expect(this.searchResults.facets.user[1]).toBeDefined();
      expect(this.searchResults.facets.user[1].key).toEqual('GoogleforWork');
      expect(this.searchResults.hits.length).toBeGreaterThan(1);
      expect(this.searchResults.hits.length).toEqual(8);
      expect(this.searchResults.hits[0].id).toEqual('3VKiNd');
      expect(this.searchResults.hits[3].id).toEqual('1NsXUW');
      expect(this.searchResults.hits[4].id).toEqual('3swrN');
    });
  });

  it('should be able to filter search results by user', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['search']
        },
        'facets': ['user','tags'],
        'filter': {
          'user': ['GoogleforWork']
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
      expect(this.searchResults.hits[0].id).toEqual('4EaEkI');
    });
  });

  it('should be able to filter search results by tag', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['search']
        },
        'facets': ['user','tags'],
        'filter': {
          'tags': ['search']
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults.hits.length).toEqual(5);
      expect(this.searchResults.hits[0].id).toEqual('3VKiNd');
      expect(this.searchResults.hits[3].id).toEqual('3swrN');
      expect(this.searchResults.hits[4].id).toEqual('2PHH0R');
    });
  });


  it('should be able to search on tokens that are only found in metadata', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['eklem']
        },
        'facets': ['user','tags']
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults.hits.length).toEqual(64);
      expect(this.searchResults.hits[0].id).toEqual('tLl7B');
      expect(this.searchResults.hits[7].id).toEqual('4B0dV8');
      expect(this.searchResults.hits[8].id).toEqual('cwHKO');
    });
  });


});
