var fs = require('fs');
var logger = require('../../lib/logger.js');
var si = require('../../lib/search-index.js');
var sitwo = require('../../lib/search-index.js');
var level = require('level');
var newindexes = level('newsi', {valueEncoding: 'json'});


describe('replication', function () {
  var data = JSON.parse(fs.readFileSync('test/testdata/justOne.json'));

  it('should open a new index', function () {
    runs(function() {
      this.openingMsg = '';
      var that = this;
      sitwo.open('sitwo', function(openingMsg) {
        that.openingMsg = openingMsg;  
      });  
    });
    waitsFor(function() {
      return this.openingMsg != '';
    }, 'message returned from search-index', 100000)
    runs(function () {
      expect(this.openingMsg).toEqual('index opened');
    });
  });
/*
  it('should open a new index', function () {
    runs(function() {
      this.openingMsg = '';
      var that = this;
      si.open('si', function(openingMsg) {
        that.openingMsg = openingMsg;  
      });  
    });
    waitsFor(function() {
      return this.openingMsg != '';
    }, 'message returned from search-index', 100000)
    runs(function () {
      expect(this.openingMsg).toEqual('index opened');
    });
  });
*/
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
      this.indexPiped = false;
      var that = this;
      si.createSnapShot(function(rs) {
        var ws = newindexes.createWriteStream();
        rs.pipe(ws).on('close', function(){
          console.log('index piped');
          that.indexPiped = true;
        });
      });
    });
    waitsFor(function() {
      return this.indexPiped;
    }, 'waiting for search results', 60000)
    runs(function() {
      expect(this.indexPiped).toEqual(true);
    });
  });


});

