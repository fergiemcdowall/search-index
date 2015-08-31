/* global it */
/* global describe */

var logLevel = 'error';
if (process.env.NODE_ENV == 'TEST') logLevel = 'info';
var should = require('should');

describe('deleting: ', function () {
  it('should index test data into the index', function (done) {
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
      },
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
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-delete-test',
                                logLevel: logLevel});
    si.add(data1, {batchName: 'data1'}, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);
        done();
      });
    });
  }),
  it('should be able to return all documents in index', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-delete-test',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['*']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(4);
      searchResults.totalHits.should.be.exactly(4);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should be able to delete a document without throwing errorness', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-delete-test',
                                logLevel: logLevel});
    si.del('2', function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should be able to return all documents in index, with one document deleted', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-delete-test',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['*']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(3);
      searchResults.totalHits.should.be.exactly(3);
      searchResults.hits[0].id.should.be.exactly('1');
      searchResults.hits[1].id.should.be.exactly('3');
      searchResults.hits[2].id.should.be.exactly('4');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should index duplicate test data into the index', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-delete-test',
                                logLevel: logLevel});
    var data2 = [
      {
        id: 1,
        name: 'The First Doc',
        test: 'this is the first doc'
      }
    ];
    si.add(data2, {batchName: 'data2'}, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should return 3 docs, since the previously indexed doc is a duplicate', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-delete-test',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['*']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(3);
      searchResults.totalHits.should.be.exactly(3);
      searchResults.hits[0].id.should.be.exactly('1');
      searchResults.hits[1].id.should.be.exactly('3');
      searchResults.hits[2].id.should.be.exactly('4');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });
});
