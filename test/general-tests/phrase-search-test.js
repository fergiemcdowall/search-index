/* global it */
/* global describe */

var logLevel = 'error';
if (process.env.NODE_ENV == 'TEST') logLevel = 'info';
var should = require('should');

describe('ngrams (phrase search): ', function () {
  var sandboxPath = 'test/sandbox';
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

  it('should index test data into the index with phrases', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests',
                                logLevel: logLevel,
                                stopwords: [],
                                nGramLength: {gte: 1, lte: 3}});
    si.add(food, {}, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able to get hits for an ngram of length 3', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['now sadly defunct']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      searchResults.hits[0].id.should.be.exactly('1');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able to get hits for documents of ngram length 2', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['tastie bite']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      searchResults.hits[0].id.should.be.exactly('1');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able to get hits for documents of ngram length 1', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['curry']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      searchResults.hits[0].id.should.be.exactly('2');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able to get hits for multiple ngrams', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['curry', 'substituted for gravy']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      searchResults.hits[0].id.should.be.exactly('2');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should index test data into the index with ngrams of only 1 and 5 (nothing in between)', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests-2',
                                logLevel: logLevel,
                                stopwords: [],
                               nGramLength: [1, 5]});
    si.add(food, {}, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able to get hits for an ngram of length 3', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests-2',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['now sadly defunct']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(0);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able to get hits for documents of ngram length 5', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests-2',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['curry sauce may be substituted']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      searchResults.hits[0].id.should.be.exactly('2');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able to get hits for documents with a search string of ngram length 5 and 1', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests-2',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['curry sauce may be substituted', 'gravy']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      searchResults.hits[0].id.should.be.exactly('2');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be not able to get hits for documents with a search string of ngram length 5, 3 and 1', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests-2',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['curry sauce may be substituted', 'and curry sauce', 'gravy']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(0);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should index test data with ngram length per field', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests-3',
                                logLevel: logLevel,
                                stopwords: []});
    var ops = {};
    ops.fieldOptions = [
      {fieldName: 'name', nGramLength: {gte: 1, lte: 3}},
      {fieldName: 'body', nGramLength: 1}
    ];
    si.add(food, ops, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able to do fielded ngrams', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests-3',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['chips and curry']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      searchResults.hits[0].id.should.be.exactly('2');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able to do fielded ngrams', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests-3',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['substituted for gravy']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(0);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able to do fielded ngrams', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-phrase-tests-3',
                                logLevel: logLevel});
    var q = {};
    q.query = {'*': ['substituted']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      searchResults.hits[0].id.should.be.exactly('2');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

});
