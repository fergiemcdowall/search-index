var fs = require('fs');
var si = require('../../')({logLevel:false});


describe('deleting and reindexing', function () {

  var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/full/reuters-000.json'));


  it('should be able to delete documents from index', function () {    
    runs(function () {
      this.err = undefined;
      this.done = false;
      var that = this;
      si.del(747, function(err) {
        that.err = err;
        that.done = true;
      });
    });
    waitsFor(function() {
      return this.done;
    }, 'waiting for indexData response', 1000)
    runs(function() {
      expect(true).toEqual(true);
    });
  });



  it('should verify delete', function () {    
    runs(function () {
      this.err = undefined;
      var that = this;
      si.get(747, function(err, doc) {
        that.err = err;
      });
    });
    waitsFor(function() {
      return this.err != undefined;
    }, 'waiting for response', 1000)
    runs(function() {
      expect(this.err['DELETE-DOCUMENT~747~*']).toBeUndefined();
      expect(this.err['DELETE-DOCUMENT~747~body']).toBeUndefined();
      expect(this.err['DELETE-DOCUMENT~747~date']).toBeUndefined();
      expect(this.err['DELETE-DOCUMENT~747~places']).toBeUndefined();
      expect(this.err['DELETE-DOCUMENT~747~title']).toBeUndefined();
      expect(this.err['VECTOR~*~747~']).toBeUndefined();
      expect(this.err['VECTOR~body~747~']).toBeUndefined();
      expect(this.err['VECTOR~date~747~']).toBeUndefined();
      expect(this.err['VECTOR~places~747~']).toBeUndefined();
      expect(this.err['VECTOR~title~747~']).toBeUndefined();
      expect(this.err['VECTOR~*fielded~747~']).toBeUndefined();
      expect(this.err['DOCUMENT~747~']).toBeUndefined();
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
      return this.value != undefined;
    }, 'TF~*~mccaw~~ should be removed from TF index ', 2000)
    runs(function () {
      expect(this.err).toEqual(null);
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


  it('deleted document is not appearing in results', function () {    
    runs(function () {
      this.searchResults = undefined;
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
      return this.searchResults != undefined;
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toBeGreaterThan(1);
      expect(this.searchResults.hits.length).toEqual(100);
      expect(this.searchResults.hits[3].id).toEqual('417');
      expect(this.searchResults.hits[12].id).toEqual('455');
      expect(this.searchResults.hits[13].id).toEqual('31');
      expect(this.searchResults.hits[16].id).toEqual('77');
    });
  });


  it('should reindex deleted document', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      var singleDoc = [];
      singleDoc.push(data['746']);      
      var options = {};
      options['batchString'] = singleDoc;
      options['batchName'] = 'justOneDoc';
      options['filters'] = ['places'];
      si.add({'batchName': 'justOneDoc', 'filters': ['places']}, singleDoc, function(err) {
        that.err = err;
        that.done = true;
      });  
    });
    waitsFor(function() {
      return this.done != false;
    }, 'err not to be true', 1000)
    runs(function () {
      expect(this.err).toEqual(null);
    });
  });


  it('document reappears in search', function () {    
    runs(function () {
      this.searchResults = undefined;
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
      return this.searchResults != undefined;
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toBeGreaterThan(1);
      expect(this.searchResults.hits.length).toEqual(100);
      expect(this.searchResults.hits[3].id).toEqual('417');
      expect(this.searchResults.hits[12].id).toEqual('455');
      expect(this.searchResults.hits[13].id).toEqual('31');
      expect(this.searchResults.hits[16].id).toEqual('77');
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

