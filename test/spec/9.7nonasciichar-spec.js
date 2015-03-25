//see http://stackoverflow.com/questions/14289323/nodejs-module-with-config-instantiate-from-require-or-from-module-exports


var fs = require('fs');
var sione = require('../../')({indexPath:'sione', logLevel:false});


describe('indexing and search', function () {

  var data1 = [
    {
      'id': 1,
      'names':'ståle synnøve Kjærsti',
      'test':'this doc should give hits for peaches peaches all of the tokens in the names field'
    },
    {
      'id': 2,
      'names':'Gerät Grünnerløkka',
      'test':'everything in names field should be searchable searchable searchable'
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



  it('should be able to search in sione without pollution from sitwo', function () {
    runs(function () {
      this.searchResults = '';
      var that = this;
      sione.search({
        'query': {
          '*': ['names']
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


});
