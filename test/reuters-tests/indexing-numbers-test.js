/* global it */
/* global describe */

var should = require('should');
var sandboxPath = 'test/sandbox';
var fs = require('fs');

describe('Indexing numeric fields, Reuters: ', function () {

  it('should index one file of test data', function (done) {
    this.timeout(20000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10-2',
                                logLevel: 'error'});
    var opt = {};
    opt.batchName = 'reuters';
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('verifies calibration of number after batch is indexed', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10-2',
                                logLevel: 'error'});
    si.indexValue({key:'TF￮randomNumber￮2749￮￮'}, function (err, value) {
      (err === null).should.be.exactly(true);
      value.length.should.be.exactly(1);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should verify indexing', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10-2',
                                logLevel: 'error'});
    si.tellMeAboutMySearchIndex(function (info) {
      should.exist(info);
      (info.totalDocs).should.be.exactly(10);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able to search number fields in indexed datas', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10-2',
                                logLevel: 'error'});
    var q = {};
    q.query = {randomNumber: [2749]};
    si.search(q, function (err, results) {
      should.exist(results);
      (err === null).should.be.exactly(true);
      results.hits.length.should.be.exactly(1);
      results.hits[0].id.should.be.exactly('9');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

});
