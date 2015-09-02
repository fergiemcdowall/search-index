/* global it */
/* global describe */

var should = require('should');
var fs = require('fs');

describe('Indexing Reuters without IDs: ', function () {
  describe('indexing reuters-000.json', function () {
    var data = [];
    var sandboxPath = 'test/sandbox';
    it('should find the data and set up a sandbox', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-no-ids',
                                  logLevel: 'error'});
      data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/noIDs/reuters-000.json'));
      data.length.should.be.exactly(1000);
      should.not.exist(data.id);
      try {
        var stats = fs.lstatSync(sandboxPath);
        stats.isDirectory().should.be.exactly(true);
      }
      catch (e) {
        console.log(e);
        true.should.be.exactly(false);
      }
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    }),
    it('should index one file of test data that doesnt contain IDs', function (done) {
      this.timeout(60000);
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-no-ids',
                                  logLevel: 'error'});
      var opt = {};
      opt.batchName = 'reuters no ids';
      si.add(data, opt, function (err) {
        (err === null).should.be.exactly(true);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should verify indexing', function (done) {
      this.timeout(10000);
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-no-ids',
                                  logLevel: 'error'});
      si.tellMeAboutMySearchIndex(function (info) {
        should.exist(info);
        (info.totalDocs).should.be.exactly(1000);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('verifies recalibration', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-no-ids',
                                  logLevel: 'error'});
      si.indexValue({key:'TF￮*￮1987￮￮'}, function (err, value) {
        (err === null).should.be.exactly(true);
        value.length.should.be.exactly(1000);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should search on all fields and get results', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-no-ids',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['usa']};  //TODO: add error message if this is
      //      not an array
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.above(1);
        searchResults.hits.length.should.be.exactly(100);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to handle multiword searches', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-no-ids', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['reuter', '1987']};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(100);
        searchResults.totalHits.should.be.exactly(922);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });
  });
});
