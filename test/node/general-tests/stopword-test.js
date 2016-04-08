/* global it */
/* global describe */

var logLevel = 'error'
if (process.env.NODE_ENV === 'TEST') logLevel = 'info'
const should = require('should')
const searchindex = require('../../../')
const tv = require('term-vector')
const sandboxPath = 'test/sandbox'

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
    }]
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
    }]

  var si, siNO, siFood, siFood2

  it('should initialize search index', function (done) {
    searchindex(
      {indexPath: sandboxPath + '/si-stopwords-test-en',
       logLevel: logLevel},
      function (err, thisSi) {
        if (err) false.should.eql(true)
        si = thisSi
        done()
      })
  })

  it('should initialize norwegian search index', function (done) {
    searchindex(
      {indexPath: sandboxPath + '/si-stopwords-test-no',
       logLevel: logLevel,
       stopwords: tv.getStopwords('no')
      },
      function (err, thisSi) {
        if (err) false.should.eql(true)
        siNO = thisSi
        done()
      })
  })

  it('should initialize a food search index', function (done) {
    searchindex(
      {indexPath: sandboxPath + '/si-stopwords-test-food',
       logLevel: logLevel,
       stopwords: []},
      function (err, thisSi) {
        if (err) false.should.eql(true)
        siFood = thisSi
        done()
      })
  })

  it('should initialize a food search index', function (done) {
    searchindex(
      {indexPath: sandboxPath + '/si-stopwords-test-food-2',
       logLevel: logLevel},
      function (err, thisSi) {
        if (err) false.should.eql(true)
        siFood2 = thisSi
        done()
      })
  })



  it('should index test data into the index with default (english) stopwords', function (done) {
    si.add(data, {batchName: 'data1'}, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should be able to return all documents that contain "dette" if indexing with english stopwords', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['dette']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(3)
      searchResults.totalHits.should.be.exactly(3)
      done()
    })
  })

  it('should index test data into the index with norwegian stopwords', function (done) {
    siNO.add(data, {batchName: 'data1'}, function (err) {
      ;(err === null).should.be.exactly(true)
      done()
    })
  })

  it('should be able to return all documents in index', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['tur']}
    }
    siNO.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      done()
    })
  })

  it('"dette" should not give any results since it is blocked by the norwegian stopwords', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['dette']}
    }
    siNO.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(0)
      done()
    })
  })

  it('should create an index of fast food without stopwords', function (done) {
    var sandboxPath = 'test/sandbox'
    siFood.add(food, {batchName: 'data1'}, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should be able to return results for "fish and chips"', function (done) {
    var q = {}
    q.query = {
      AND: {'*': 'fish and chips'.split(' ')}
    }
    siFood.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      done()
    })
  })

  it('should be able to return results for "and"', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['and']}
    }
    siFood.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(2)
      searchResults.totalHits.should.be.exactly(2)
      done()
    })
  })

  it('should create an index of fast food without stopwords', function (done) {
    siFood2.add(food, {
      batchName: 'food',
      stopwords: []
    }, function (err) {
      ;(err === null).should.be.exactly(true)
      done()
    })
  })

  it('should be able to return "fish and chips"', function (done) {
    var q = {}
    q.query = {
      AND: {'*': 'fish and chips'.split(' ')}
    }
    siFood2.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      done()
    })
  })
})
