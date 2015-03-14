var fs = require('fs');
var si = require('../../')({indexPath:'si-world-bank'});
var _ = require('lodash');

describe('indexing and search', function () {

  var padInt = function(intIn) {
    return ("000000000000000" + intIn).slice(-15);
  }

  var data = JSON.parse(fs.readFileSync('node_modules/world-bank-dataset/world-bank-projects.json'));

  var processDoc = function(datum) {
    delete datum._id;
    delete datum.projectdocs;
    delete datum.theme1;
    delete datum.majorsector_percent;
    delete datum.mjsector_namecode;
    delete datum.mjtheme_namecode;
    delete datum.sector;
    delete datum.sector_namecode;
    delete datum.sector1;
    delete datum.sector2;
    delete datum.theme_namecode;
    datum.totalamt = [padInt(datum.totalamt)];
    return datum;
  };

//  console.log(_.map(data, processDoc));


  it('should index one file of test data', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      si.add({'batchName': 'world-bank-projects.json',
              'filters': ['mjtheme', 'totalamt']},
             _.map(data, processDoc), function(err) {
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
          '*': ['ethiopia']
        },
        'facets': {'mjtheme':{},'totalamt':{}}
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toEqual(4);
      expect(this.searchResults.facets[0].value).toBeDefined();
      expect(this.searchResults.facets[0].value[0].key).toEqual('Human development');
      expect(this.searchResults.facets[0].value[0].value).toEqual(2);
      expect(this.searchResults.facets[0].value.length).toEqual(7);
      expect(this.searchResults.hits[0].id).toEqual('P129828');
      expect(this.searchResults.hits[1].id).toEqual('P123531');
      expect(this.searchResults.hits[2].id).toEqual('P117731');
      expect(this.searchResults.hits[3].id).toEqual('P128891');
    });
  });


  it('faceting for totalamt should work even though input is not in array format', function () {
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['ethiopia']
        },
        'facets': {'totalamt':{}}
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
      console.log(this.searchResults.facets);
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toEqual(4);
      expect(this.searchResults.facets[0].value).toBeDefined();
      expect(this.searchResults.facets[0].value[0].key).toEqual(padInt('100000000'));
      expect(this.searchResults.facets[0].value[0].value).toEqual(1);
      expect(this.searchResults.facets[0].value[1].key).toEqual(padInt('130000000'));
      expect(this.searchResults.facets[0].value[1].value).toEqual(1);
      expect(this.searchResults.facets[0].value[2].key).toEqual(padInt('415000000'));
      expect(this.searchResults.facets[0].value[2].value).toEqual(1);
      expect(this.searchResults.facets[0].value[3].key).toEqual(padInt('600000000'));
      expect(this.searchResults.facets[0].value[3].value).toEqual(1);
      expect(this.searchResults.facets[0].value.length).toEqual(4);
    });
  });

});
