var fs = require('fs');
var logger = require('../../lib/logger.js');
var si = require('../../lib/search-index.js');
var level = require('level');
var newindexes = level('newsi', {valueEncoding: 'json'});


describe('replication', function () {
  it('should be able to create a readStream', function () {    
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

