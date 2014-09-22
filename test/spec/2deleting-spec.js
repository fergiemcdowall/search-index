var fs = require('fs');
var logger = require('../../lib/logger.js');
var si = require('../../lib/search-index.js');


describe('deleting and reindexing', function () {

  var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/reuters-000.json'));


  it('should be able to delete documents from index', function () {    
    runs(function () {
      this.res = '';
      var that = this;
      si.del(747, function(res) {
        that.res = res;
      });
    });
    waitsFor(function() {
      return this.res != '';
    }, 'waiting for indexData response', 5000)
    runs(function() {
      logger.debug(this.res);
      expect(true).toEqual(true);
    });
  });


  it('should verify delete', function () {    
    runs(function () {
      this.res = '';
      var that = this;
      si.get(747, function(res) {
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

  it('verifies recalibration after delete', function () {
    runs(function() {
      this.value = undefined;
      this.err = undefined;
      var that = this;
      si.indexValue({key:'TF~*~mccaw~~'}, function(err, value) {
        that.value = value;
        that.err = err;
      });
    });
    waitsFor(function() {
      return this.err != undefined;
    }, 'TF~*~mccaw~~ should be removed from TF index ', 2000)
    runs(function () {
      expect(this.err).toEqual(true);
    });
  });

  it('verifies recalibration after delete', function () {
    runs(function() {
      this.value = undefined;
      this.err = undefined;
      var that = this;
      si.indexValue({key:'TF~*~1987~~'}, function(err, value) {
        that.value = value;
      });
    });
    waitsFor(function() {
      return this.value != undefined;
    }, 'TF~*~1987~~ should have a value of 999 in TF index ', 2000)
    runs(function () {
      expect(this.value.length).toEqual(999);
    });
  });

  /*
  it('verifies recalibration after delete', function () {
    runs(function() {
      this.value = '';
      var that = this;
      si.indexValue('TF~*~1987~~', function(value) {
        that.value = value;
      });
    });
    waitsFor(function() {
      return this.value != '';
    }, 'TF~*~1987~~ should have a value of 999 in TF index ', 100000)
    runs(function () {
      expect(this.value.length).toEqual(999);
    });
  });
  */

  it('deleted document is not appearing in results', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        'query': {
          '*': ['usa']
        }
      }, function(searchResults) {
        logger.debug(searchResults);
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
      this.err = undefined;
      var that = this;
      var singleDoc = {};
      singleDoc['747'] = data['747'];      
      logger.debug(singleDoc);

      var options = {};
      options['batchString'] = singleDoc;
      options['batchName'] = 'justOneDoc';
      options['filters'] = ['places'];

      //      si.add(singleDoc, 'justOneDoc', ['places'], function(err) {
      si.add({'batchName': 'justOneDoc', 'filters': ['places']}, singleDoc, function(err) {
        that.err = err;
      });  
    });
    waitsFor(function() {
      return this.err != undefined;
    }, 'err not to be true', 5000)
    runs(function () {
      expect(this.err).toEqual(false);
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

  it('verifies recalibration after document is added again', function () {
    runs(function() {
      this.err = undefined;
      this.value = undefined;
      var that = this;
      si.indexValue({key:'TF~*~mccaw~~'}, function(err, value) {
        that.value = value;
      });
    });
    waitsFor(function() {
      return this.value != undefined;
    }, 'TF~*~mccaw~~ should be present in TF index ', 2000)
    runs(function () {
      expect(this.value[0]).toEqual('747');
    });
  });

  it('verifies recalibration after document is added again', function () {
    runs(function() {
      this.err = undefined;
      this.value = undefined;
      var that = this;
      si.indexValue({key:'TF~*~1987~~'}, function(err, value) {
        that.value = value;
      });
    });
    waitsFor(function() {
      return this.value != undefined;
    }, 'TF~*~1987~~ should have a length of 1000 in TF index ', 2000)
    runs(function () {
      expect(this.value.length).toEqual(1000);
    });
  });

});

