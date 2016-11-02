/* global it */
/* global describe */

const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const should = require('should')
const stopwords = require('stopword').en

const getDataStream = function () {
  var s = new Readable({ objectMode: true })
  s.push({
    id: 1,
    name: 'Fish and chips',
    test: 'The best fish and chips were from the now sadly defunct Tastie Bite'
  })
  s.push({
    id: 2,
    name: 'Chips and curry sauce',
    test: 'A classic, the curry sauce may be substituted for gravy'
  })
  s.push({
    id: 3,
    name: 'Haggisxandxchips',
    test: 'Axseldomxdelicacy'
  })
  s.push(null)
  return s
}

describe('ngrams (phrase search): ', function () {
  var si, si2, si3, si4

  it('should initialize search index', function (done) {
    SearchIndex({
      indexPath: sandboxPath + '/si-phrase-tests',
      logLevel: logLevel,
      stopwords: stopwords
    },
      function (err, thisSi) {
        if (err) false.should.eql(true)
        si = thisSi
        getDataStream()
          .pipe(si.defaultPipeline({
            stopwords: [],
            nGramLength: {gte: 1, lte: 3}
          }))
          .pipe(si.add())
          .on('data', function (data) {

          })
          .on('end', function () {
            true.should.be.exactly(true)
            return done()
          })
      })
  })

  it('should initialize search index', function (done) {
    SearchIndex({
      indexPath: sandboxPath + '/si-phrase-tests-2',
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si2 = thisSi
      getDataStream()
        .pipe(si2.defaultPipeline({
          stopwords: [],
          nGramLength: [1, 5]
        }))
        .pipe(si2.add())
        .on('data', function (data) {

        })
        .on('end', function () {
          true.should.be.exactly(true)
          return done()
        })
    })
  })

  it('should initialize search index', function (done) {
    SearchIndex(
      {indexPath: sandboxPath + '/si-phrase-tests-3',
        logLevel: logLevel,
      stopwords: []},
      function (err, thisSi) {
        if (err) false.should.eql(true)
        si3 = thisSi
        getDataStream()
          .pipe(si3.defaultPipeline({
            fieldOptions: {
              name: {
                nGramLength: {gte: 1, lte: 3}
              },
              body: {
                nGramLength: 1
              }
            }
          }))
          .pipe(si3.add())
          .on('data', function (data) {

          })
          .on('end', function () {
            true.should.be.exactly(true)
            return done()
          })
      })
  })

  it('should initialize search index', function (done) {
    SearchIndex(
      {indexPath: sandboxPath + '/si-phrase-tests-4',
        logLevel: logLevel,
      stopwords: []},
      function (err, thisSi) {
        if (err) false.should.eql(true)
        si4 = thisSi
        getDataStream()
          .pipe(si4.defaultPipeline({
            fieldOptions: {
              name: {
                separator: 'x'
              }
            }
          }))
          .pipe(si4.add())
          .on('data', function (data) {

          })
          .on('end', function () {
            true.should.be.exactly(true)
            return done()
          })
      })
  })

  it('should be able to get hits for an ngram of length 3', function (done) {
    var results = [{
      id: 1,
      name: 'Fish and chips',
      test: 'The best fish and chips were from the now sadly defunct Tastie Bite'
    }]
    si.search({
      query: [{
        AND: {'*': ['now sadly defunct']}
      }]
    }).on('data', function (data) {
      data.document.should.eql(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('should be able to get hits for documents of ngram length 2', function (done) {
    var results = [{
      id: 1,
      name: 'Fish and chips',
      test: 'The best fish and chips were from the now sadly defunct Tastie Bite'
    }]
    si.search({
      query: [{
        AND: {'*': ['tastie bite']}
      }]
    }).on('data', function (data) {
      data.document.should.eql(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('should be able to get hits for documents of ngram length 1', function (done) {
    var results = [{
      id: 2,
      name: 'Chips and curry sauce',
      test: 'A classic, the curry sauce may be substituted for gravy'
    }]
    si.search({
      query: [{
        AND: {'*': ['curry']}
      }]
    }).on('data', function (data) {
      data.document.should.eql(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('should be able to get hits for multiple ngrams', function (done) {
    var results = [{
      id: 2,
      name: 'Chips and curry sauce',
      test: 'A classic, the curry sauce may be substituted for gravy'
    }]
    si.search({
      query: [{
        AND: {'*': ['curry', 'substituted for gravy']}
      }]
    }).on('data', function (data) {
      data.document.should.eql(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('should not be able to get hits for an ngram of length 3 when indexing with ngram [1, 5]', function (done) {
    var i = 0
    si2.search({
      query: [{
        AND: {'*': ['now sadly defunct']}
      }]
    }).on('data', function (data) {
      i++
    }).on('end', function () {
      i.should.be.exactly(0)
      return done()
    })
  })

  it('should be able to get hits for documents of ngram length 5 ([1, 5])', function (done) {
    var results = [{
      id: 2,
      name: 'Chips and curry sauce',
      test: 'A classic, the curry sauce may be substituted for gravy'
    }]
    si2.search({
      query: [{
        AND: {'*': ['curry sauce may be substituted']}
      }]
    }).on('data', function (data) {
      data.document.should.eql(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('should be able to get hits for documents with a search string of ngram length 5 and 1', function (done) {
    var results = [{
      id: 2,
      name: 'Chips and curry sauce',
      test: 'A classic, the curry sauce may be substituted for gravy'
    }]
    si2.search({
      query: [{
        AND: {'*': ['curry sauce may be substituted', 'gravy']}
      }]
    }).on('data', function (data) {
      data.document.should.eql(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('should be not able to get hits for documents with a search string of ngram length 5, 3 and 1', function (done) {
    var i = 0
    si2.search({
      query: [{
        AND: {'*': ['curry sauce may be substituted', 'and curry sauce', 'gravy']}
      }]
    }).on('data', function (data) {
      i++
    }).on('end', function () {
      i.should.be.exactly(0)
      return done()
    })
  })

  it('should be able to do fielded ngrams', function (done) {
    var results = [{
      id: 2,
      name: 'Chips and curry sauce',
      test: 'A classic, the curry sauce may be substituted for gravy'
    }]
    si3.search({
      query: [{
        AND: {'name': ['chips and curry']}
      }]
    }).on('data', function (data) {
      data.document.should.eql(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('should be able to do fielded ngrams', function (done) {
    var i = 0
    si3.search({
      query: [{
        AND: {'test': ['substituted for gravy']}
      }]
    }).on('data', function (data) {
      i++
    }).on('end', function () {
      i.should.be.exactly(0)
      return done()
    })
  })

  it('should be able to do fielded ngrams', function (done) {
    var results = [{
      id: 2,
      name: 'Chips and curry sauce',
      test: 'A classic, the curry sauce may be substituted for gravy'
    }]
    si3.search({
      query: [{
        AND: {'test': ['substituted']}
      }]
    }).on('data', function (data) {
      data.document.should.eql(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('should be able to confirm field level separator', function (done) {
    var results = [{
      id: 3,
      name: 'Haggisxandxchips',
      test: 'Axseldomxdelicacy'
    }]
    si4.search({
      query: [{
        AND: {'name': ['chips']}
      }]
    }).on('data', function (data) {
      data.document.should.eql(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })
})
