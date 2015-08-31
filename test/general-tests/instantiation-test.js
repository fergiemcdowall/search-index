/* global it */
/* global describe */

var should = require('should');
var sandboxPath = 'test/sandbox';

describe('Instantiation: ', function () {
  describe('setting up different indexes with no pollution', function () {
    var data1 = [
    {
      id: 1,
      name: 'The First Doc',
      test: 'this is the first doc'
    },
    {
      id: 2,
      name: 'The Second Doc',
      test: 'this is the second doc'
    }];

    var data2 = [
    {
      id: 3,
      name: 'The Third Doc',
      test: 'this is the third doc'
    },
    {
      id: 4,
      name: 'The Fourth Doc',
      test: 'this is the fourth doc'
    }];

    it('should index test data into the first index', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-init-one',
                                  logLevel: 'error'});
      si.add(data1, {}, function (err) {
        (err === null).should.be.exactly(true);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });

    it('should index test data into the second index', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-init-two',
                                  logLevel: 'error'});
      si.add(data2, {}, function (err) {
        (err === null).should.be.exactly(true);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });

    it('should be able to search in si-init-one without pollution from si-init-two', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-init-one',
                                  logLevel: 'error'});
      var q = {};
      q.query = {'*': ['*']};
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(2);
        results.totalHits.should.be.exactly(2);
        results.hits[0].id.should.be.exactly('1');
        results.hits[1].id.should.be.exactly('2');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });

    it('should be able to search in si-init-two without pollution from si-init-one', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-init-two',
                                  logLevel: 'error'});
      var q = {};
      q.query = {'*': ['*']};
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(2);
        results.totalHits.should.be.exactly(2);
        results.hits[0].id.should.be.exactly('3');
        results.hits[1].id.should.be.exactly('4');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });
  });
});
