var fs = require('fs');
var logger = require('../../lib/logger.js');
var si = require('../../lib/search-index.js');
var level = require('level');

describe('replication', function () {
  var data = JSON.parse(fs.readFileSync('test/testdata/justOne.json'));

//should jeust overwrite if test is being run as part of a full suite
  it('should index one file of test data', function () {
    runs(function() {
      this.indexingMsg = '';
      var that = this;
      si.add(data, 'reuters-000.json', ['places'], function(indexingMsg) {
        that.indexingMsg = indexingMsg;  
      });  
    });
    waitsFor(function() {
      return this.indexingMsg != '';
    }, 'indexingMsg not to be empty (search results returned)', 30000)
    runs(function () {
      expect(this.indexingMsg).toEqual('[success] indexed batch: reuters-000.json');
    });
  });

  it('should be able to create a snapshot', function () {    
    runs(function () {
      this.completed = false;
      var that = this;
      si.createSnapShot(function(rs) {
        that.completed = true;
      });
    });
    waitsFor(function() {
      return this.completed;
    }, 'waiting for search results', 60000)
    runs(function() {
      expect(this.completed).toEqual(true);
    });
  });


  it('should empty the index', function () {    
    runs(function () {
      this.completed = false;
      var that = this;
      si.empty(function(msg){
        that.completed = true;
      });
    });
    waitsFor(function() {
      return this.completed;
    }, 'waiting for response...', 60000)
    runs(function() {
      expect(this.completed).toEqual(true);
    });
  });

  it('should be able to refeed from a snapshot', function () {    
    runs(function () {
      this.completed = false;
      var that = this;
      si.replicate(function(msg){
        that.completed = true;
      });
    });
    waitsFor(function() {
      return this.completed;
    }, 'waiting for search results', 60000)
    runs(function() {
      expect(this.completed).toEqual(true);
    });
  });


});

