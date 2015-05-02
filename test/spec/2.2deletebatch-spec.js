var fs = require('fs');
var si = require('../../')({logLevel:false});
var _ = require('lodash');

describe('deleting a batch', function () {

  var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));

  it('should index one file of test data', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      si.add({'batchName': 'reuters-000.json', 'filters': ['places']}, data, function(err) {
        that.err = err;
        that.done = true;
      });
    });
    waitsFor(function() {
      return this.done != false;
    }, 'err not to be empty (search err returned)', 60000)
    runs(function () {
      expect(this.err).toEqual(null);
    });
  });


  it('it should delete documents 1, 3, 5, 7, 10', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      si.deleteBatch(['7', '10', '5', '1', '3'], function(err) {
        that.err = err;
        that.done = true;
      });
    });
    waitsFor(function() {
      return this.done != false;
    }, 'err not to be empty (search err returned)', 60000)
    runs(function () {
      expect(this.err).toEqual(null);
    });
  });

  it('should be able to create a snapshot', function () {
    runs(function () {
      console.log('snapshot');
      this.completed = false;
      this.error = false;
      var that = this;
      si.snapShot(function(rs) {
        rs.pipe(fs.createWriteStream('backup1.gz'))
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
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.completed).toEqual(true);
      expect(this.error).toEqual(false);
    });
  });


});
