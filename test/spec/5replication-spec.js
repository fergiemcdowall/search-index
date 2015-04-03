var fs = require('fs');
var siJustten = require('../../')({indexPath:'justTen', logLevel:false});
var level = require('levelup');

describe('replication', function () {
  var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));


  it('should index one file of test data', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      siJustten.add({'batchName': 'just10'},
             data, function(err) {
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


  it('should be able to create a snapshot', function () {    
    runs(function () {
      this.completed = false;
      this.error = false;
      var that = this;
      siJustten.snapShot(function(rs) {
        rs.pipe(fs.createWriteStream('backup.gz'))
          .on('close', function() {
            that.completed = true;
          })
          .on('error', function() {
            that.error = true;
          });
      });
    });
    waitsFor(function() {
      return this.completed;
    }, 'waiting for search results', 60000)
    runs(function() {
      expect(this.completed).toEqual(true);
      expect(this.error).toEqual(false);
    });
  });


  it('should empty the index', function () {    
    runs(function () {
      this.completed = false;
      this.error = false;
      var that = this;
      siJustten.empty(function(err){
        that.completed = true;
        that.error = err;
      });
    });
    waitsFor(function() {
      return this.completed;
    }, 'waiting for response...', 5000)
    runs(function() {
      expect(this.completed).toEqual(true);
      expect(this.error).toEqual(null);
    });
  });


  it('should index one file of test data', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      siJustten.add({'batchName': 'just10'},
             data, function(err) {
        that.err = err;
        that.done = true;
      });
    });
    waitsFor(function() {
      return this.done != false;
    }, 'err not to be empty (search err returned)', 5000)
    runs(function () {
      expect(this.err).toEqual(null);
    });
  });


  it('should be able to display information about the index', function () {    
    runs(function () {
      this.tellMeAboutMySearchIndexResponse = '';
      var that = this;
      siJustten.tellMeAboutMySearchIndex(function(tellMeAboutMySearchIndexResponse) {
        that.tellMeAboutMySearchIndexResponse = tellMeAboutMySearchIndexResponse;
      });
    });
    waitsFor(function() {
      return this.tellMeAboutMySearchIndexResponse != '';
    }, 'waiting for indexData response', 1000)
    runs(function() {
      expect(this.tellMeAboutMySearchIndexResponse.totalDocs).toEqual(10);
    });
  });


  it('should be able to refeed from a snapshot', function () {    
    runs(function () {
      this.completed = false;
      var that = this;
      siJustten.replicate(fs.createReadStream('backup.gz'), function(msg){
        that.completed = true;
      });
    });
    waitsFor(function() {
      return this.completed;
    }, 'waiting for search results', 30000)
    runs(function() {
      expect(this.completed).toEqual(true);
    });
  });


});
