var fs = require('fs');
var logger = require('../../lib/logger.js');
var si = require('../../lib/search-index.js');
var level = require('level');

describe('replication', function () {
  var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justOne.json'));

//should jeust overwrite if test is being run as part of a full suite
  it('should index one file of test data', function () {
    runs(function() {
      this.err = 'NOTSET';
      var that = this;
      var options = {};
      options['batchName'] = 'justOne.json';
      options['filters'] = ['places'];
      si.add(options, data, function(err) {
        that.err = err;
      });

      //      si.add(data, 'justOne.json', ['places'], function(err) {
      //  that.err = err;  
      //});  
    });
    waitsFor(function() {
      return this.err != 'NOTSET';
    }, 'err not to be empty (search results returned)', 5000)
    runs(function () {
      expect(this.err).toEqual(false);
    });
  });



  it('should be able to create a snapshot', function () {    
    runs(function () {
      this.completed = false;
      var that = this;
      si.snapShot(function(rs) {
        rs.pipe(fs.createWriteStream('backup.gz'))
          .on('close', function() {
            that.completed = true;
          });
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
    }, 'waiting for response...', 5000)
    runs(function() {
      expect(this.completed).toEqual(true);
    });
  });

  it('should be able to refeed from a snapshot', function () {    
    runs(function () {
      this.completed = false;
      var that = this;
      si.replicate(fs.createReadStream('backup.gz'), function(msg){
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

