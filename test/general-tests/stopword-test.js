/* global it */
/* global describe */

var logLevel = 'error';
if (process.env.NODE_ENV == 'TEST') logLevel = 'info';
var should = require('should');

describe('stopwords: ', function () {
  var food = [
    {
      id: 1,
      name: 'Fish and chips',
      test: 'The best fish and chips were from the now sadly defunct Tastie Bite'
    },
    {
      id: 2,
      name: 'Chips and curry sauce',
      test: 'A classic, the curry sauce may be substituted for gravy'
    }];
  var data = [
    {
      id: 1,
      name: 'Norsk dokument',
      test: 'Dette er et norsk dokument'
    },
    {
      id: 2,
      name: 'Harry Handel',
      test: 'La oss dra til Svinesund for å kjøpe billige greier, dette blir kult'
    },
    {
      id: 3,
      name: 'Gulrekka',
      test: 'Jeg har et A4 liv og ser på dårlig TV på en fredag kveld'
    },
    {
      id: 4,
      name: 'Danskebåten',
      test: 'Ta en tur til Køben- dette blir stas!'
    }];
  it('should index test data into the index', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-stopwords-test-en',
                                logLevel: logLevel});
    si.add({batchName: 'data1'}, data, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should be able to return all documents in index', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-stopwords-test-en',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['dette']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(3);
      searchResults.totalHits.should.be.exactly(3);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should index test data into the index', function (done) {
    var sandboxPath = 'test/sandbox';
    var sw = require('stopword');
    var si = require('../../')({indexPath: sandboxPath + '/si-stopwords-test-no',
                                logLevel: logLevel,
                                stopwords: sw.getStopwords('no')});
    si.add({batchName: 'data1'}, data, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should be able to return all documents in index', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-stopwords-test-no',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['tur']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should be able to return all documents in index', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-stopwords-test-no',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['dette']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(0);
      //TODO: the next line should work
      //      searchResults.totalHits.should.be.exactly(0);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should create an index of fast food without stopwords', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-stopwords-test-food',
                                logLevel: logLevel,
                                stopwords: []});
    si.add({batchName: 'data1'}, food, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should be able to return results for "fish and chips"', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-stopwords-test-food',
                                logLevel: logLevel,
                                stopwords: []});
    var q = {};
    q.query = {'*': 'fish and chips'.split(' ')};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should be able to return results for "and"', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-stopwords-test-food',
                                logLevel: logLevel,
                                stopwords: []});
    var q = {};
    q.query = {'*': ['and']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(2);
      searchResults.totalHits.should.be.exactly(2);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should create an index of fast food without stopwords', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-stopwords-test-food-2',
                                logLevel: logLevel});
    si.add({batchName: 'food'}, food, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should be able to return "fish and chips"', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-stopwords-test-food-2',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': 'fish and chips'.split(' ')};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });
});
