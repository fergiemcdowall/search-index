/* global it */
/* global describe */

var logLevel = 'error'
if (process.env.NODE_ENV === 'TEST') logLevel = 'info'
var should = require('should')
var searchindex = require('../../')
const sandboxPath = 'test/sandbox'

describe('ngrams (phrase search): ', function () {
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
    }]

  var si, si2, si3

  it('should initialize search index', function (done) {
    searchindex(
      {indexPath: sandboxPath + '/si-phrase-tests',
       logLevel: logLevel,
       stopwords: [],
       nGramLength: {gte: 1, lte: 3}},
      function (err, thisSi) {
        if (err) false.should.eql(true)
        si = thisSi
        done()
      })
  })

  it('should initialize search index', function (done) {
    searchindex(
      {indexPath: sandboxPath + '/si-phrase-tests-2',
       logLevel: logLevel,
       stopwords: [],
       nGramLength: [1, 5]},
      function (err, thisSi) {
        if (err) false.should.eql(true)
        si2 = thisSi
        done()
      })
  })

  it('should initialize search index', function (done) {
    searchindex(
      {indexPath: sandboxPath + '/si-phrase-tests-3',
       logLevel: logLevel,
       stopwords: []},
      function (err, thisSi) {
        if (err) false.should.eql(true)
        si3 = thisSi
        done()
      })
  })


  it('should index test data into the index with phrases', function (done) {
    si.add(food, {}, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should be able to get hits for an ngram of length 3', function (done) {
    var q = {}
    q.query = {'*': ['now sadly defunct']}
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      searchResults.hits[0].id.should.be.exactly('1')
      done()
    })
  })

  it('should be able to get hits for documents of ngram length 2', function (done) {
    var q = {}
    q.query = {'*': ['tastie bite']}
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      searchResults.hits[0].id.should.be.exactly('1')
      done()
    })
  })

  it('should be able to get hits for documents of ngram length 1', function (done) {
    var q = {}
    q.query = {'*': ['curry']}
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      searchResults.hits[0].id.should.be.exactly('2')
      done()
    })
  })

  it('should be able to get hits for multiple ngrams', function (done) {
    var q = {}
    q.query = {'*': ['curry', 'substituted for gravy']}
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      searchResults.hits[0].id.should.be.exactly('2')
      done()
    })
  })

  it('should index test data into the index with ngrams of only 1 and 5 (nothing in between)', function (done) {
    si2.add(food, {}, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should be able to get hits for an ngram of length 3', function (done) {
    var q = {}
    q.query = {'*': ['now sadly defunct']}
    si2.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(0)
      done()
    })
  })

  it('should be able to get hits for documents of ngram length 5', function (done) {
    var q = {}
    q.query = {'*': ['curry sauce may be substituted']}
    si2.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      searchResults.hits[0].id.should.be.exactly('2')
      done()
    })
  })

  it('should be able to get hits for documents with a search string of ngram length 5 and 1', function (done) {
    var q = {}
    q.query = {'*': ['curry sauce may be substituted', 'gravy']}
    si2.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      done()
    })
  })

  it('should be not able to get hits for documents with a search string of ngram length 5, 3 and 1', function (done) {
    var q = {}
    q.query = {'*': ['curry sauce may be substituted', 'and curry sauce', 'gravy']}
    si2.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(0)
      done()
    })
  })

  it('should index test data with ngram length per field', function (done) {
    var ops = {}
    ops.fieldOptions = [
      {fieldName: 'name', nGramLength: {gte: 1, lte: 3}},
      {fieldName: 'body', nGramLength: 1}
    ]
    si3.add(food, ops, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should be able to do fielded ngrams', function (done) {
    var q = {}
    q.query = {'*': ['chips and curry']}
    si3.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      searchResults.hits[0].id.should.be.exactly('2')
      done()
    })
  })

  it('should be able to do fielded ngrams', function (done) {
    var q = {}
    q.query = {'*': ['substituted for gravy']}
    si3.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(0)
      done()
    })
  })

  it('should be able to do fielded ngrams', function (done) {
    var q = {}
    q.query = {'*': ['substituted']}
    si3.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      searchResults.hits[0].id.should.be.exactly('2')
      done()
    })
  })
})
