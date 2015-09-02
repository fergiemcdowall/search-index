/* global it */
/* global describe */

var should = require('should');
var fs = require('fs');

describe('deleting a batch: ', function () {

  it('should index one file of test data', function (done) {
    this.timeout(10000);
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10',
                                logLevel: 'error'});
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    si.add(data, {batchName: 'reuters-000.json'}, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('it should delete documents 1, 3, 5, 7, 10', function (done) {
    this.timeout(10000);
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10',
                                logLevel: 'error'});
    si.deleteBatch(['7', '10', '5', '1', '3'], function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able verify that docs are deleted', function (done) {
    this.timeout(10000);
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10',
                                logLevel: 'error'});
    si.search({query: {'*': ['*']}}, function (err, result) {
      should.exist(result);
      (err === null).should.be.exactly(true);
      result.totalHits.should.be.exactly(5);
      result.hits.length.should.be.exactly(5);
      result.hits[0].id.should.be.exactly('9');
      result.hits[1].id.should.be.exactly('4');
      result.hits[2].id.should.be.exactly('6');
      result.hits[3].id.should.be.exactly('2');
      result.hits[4].id.should.be.exactly('8');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

});
