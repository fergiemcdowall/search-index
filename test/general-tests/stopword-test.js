/* global it */
/* global describe */

var logLevel = 'error';
if (process.env.NODE_ENV == 'TEST') logLevel = 'info'
var should = require('should');

describe('stopwords: ', function () {
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
      si.close(function (err) {if (err) false.should.eql(true);done();});
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
      si.close(function (err) {if (err) false.should.eql(true);done();});
    });
  }),
  it('should index test data into the index', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-stopwords-test-no',
                                logLevel: logLevel,
                                stopwordLang: 'no'});
    si.add({batchName: 'data1'}, data, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {if (err) false.should.eql(true);done();});
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
      si.close(function (err) {if (err) false.should.eql(true);done();});
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
      si.close(function (err) {if (err) false.should.eql(true);done();});
    });
  });
});
