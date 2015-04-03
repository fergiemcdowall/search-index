//see http://stackoverflow.com/questions/14289323/nodejs-module-with-config-instantiate-from-require-or-from-module-exports


var fs = require('fs');
var sione = require('../../')({indexPath:'sione', logLevel:false});
var sitwo = require('../../')({indexPath:'sitwo', logLevel:false});

describe('indexing and search', function () {

  var data1 = [
    {
      'id':1,
      'name':'The First Doc',
      'test':'this is the first doc'
    },
    {
      'id':2,
      'name':'The Second Doc',
      'test':'this is the second doc'
    }];

  var data2 = [
    {
      'id':3,
      'name':'The Third Doc',
      'test':'this is the third doc'
    },
    {
      'id':4,
      'name':'The Fourth Doc',
      'test':'this is the fourth doc'
    }];


  it('should index test data into the first index', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      sione.add({'batchName': 'data1'}, data1, function(err) {
        that.err = err;
        that.done = true;
      });
    });
    waitsFor(function() {
      return this.done != false;
    }, 'err not to be empty (search err returned)', 1000)
    runs(function () {
      expect(this.err).toEqual(null);
    });
  });


  it('should index test data into the second index', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      sitwo.add({'batchName': 'data2'}, data2, function(err) {
        that.err = err;
        that.done = true;
      });
    });
    waitsFor(function() {
      return this.done != false;
    }, 'err not to be empty (search err returned)', 1000)
    runs(function () {
      expect(this.err).toEqual(null);
    });
  });

  it('should be able to search in sione without pollution from sitwo', function () {
    runs(function () {
      this.searchResults = '';
      var that = this;
      sione.search({
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
      expect(this.searchResults.hits.length).toEqual(2);
      expect(this.searchResults.hits[0].id).toEqual('2');
      expect(this.searchResults.hits[1].id).toEqual('1');
    });
  });

  it('should be able to search in sitwo without pollution from sione', function () {
    runs(function () {
      this.searchResults = '';
      var that = this;
      sitwo.search({
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
      expect(this.searchResults.hits.length).toEqual(2);
      expect(this.searchResults.hits[0].id).toEqual('4');
      expect(this.searchResults.hits[1].id).toEqual('3');
    });
  });

});
