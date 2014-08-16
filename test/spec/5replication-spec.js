var fs = require('fs');
var logger = require('../../lib/logger.js');
var si = require('../../lib/search-index.js');
var sitwo = require('../../lib/search-index.js');
var level = require('level');
var newindexes = level('newsi', {valueEncoding: 'json'});


describe('replication', function () {

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


  it('should be able to replicate from a snapshot', function () {    
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

