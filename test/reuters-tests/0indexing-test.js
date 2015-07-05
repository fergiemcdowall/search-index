/* global it */
/* global describe */

var assert = require('assert');
var should = require('should');
var fs = require('fs');

describe('Indexing Reuters: ', function () {
  describe('indexing reuters-000.json', function () {
    var data = [];
    var sandboxPath = 'test/sandbox';
    it('should find the data and set up a sandbox', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'error'});
      data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/full/reuters-000.json'));
      assert.equal(data.length, 1000);
      assert.equal(data[0].id, '1');
      try {
        var stats = fs.lstatSync(sandboxPath);
        assert(stats.isDirectory());
      }
      catch (e) {
        console.log(e);
        assert(false);
      }
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    }),
    it('should index the data', function (done) {
      this.timeout(120000);
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'error'});
      var opt = {};
      opt.batchName = 'reuters';
      opt.filters = ['places', 'topics'];
      si.add(opt, data, function (err) {
        (err === null).should.be.exactly(true);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should verify indexing', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'error'});
      si.tellMeAboutMySearchIndex(function (info) {
        should.exist(info);
        (info.totalDocs).should.be.exactly(1000);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });
  });
});
