/* global it */
/* global describe */

const Readable = require('stream').Readable
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const searchindex = require('../../../')
const should = require('should')
const sw = require('stopword')

should

describe('stopwords: ', function () {
  const getDataStream = function () {
    const data = [
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
    var s = new Readable({ objectMode: true })
    data.forEach(function (datum) {
      s.push(datum)
    })
    s.push(null)
    return s
  }

  const getFoodStream = function () {
    const data = [
      {
        id: 1,
        name: 'Fish and chips',
        test: 'The best fish and chips were from the now sadly defunct Tastie Bite'
      },
      {
        id: 2,
        name: 'Chips and curry sauce',
        test: 'A classic, the curry sauce may be substituted for gravy'
      }
    ]
    var s = new Readable({ objectMode: true })
    data.forEach(function (datum) {
      s.push(datum)
    })
    s.push(null)
    return s
  }

  var si, siNO, siFood

  it('should initialize search index', function (done) {
    searchindex(
      {indexPath: sandboxPath + '/si-stopwords-test-en',
      logLevel: logLevel},
      function (err, thisSi) {
        if (err) false.should.eql(true)
        si = thisSi
        getDataStream()
          .pipe(si.defaultPipeline())
          .pipe(si.add())
          .on('data', function (data) {
          })
          .on('end', function () {
            true.should.be.exactly(true)
            return done()
          })
      })
  })

  it('should be able to return all documents that contain "dette" if indexing with english stopwords', function (done) {
    var results = ['4', '2', '1']
    var i = 0
    si.search({
      query: {
        AND: {'*': ['dette']}
      }
    }).on('data', function (data) {
      results.shift().should.be.exactly(data.id)
      i++
    }).on('end', function () {
      i.should.be.exactly(3)
      return done()
    })
  })

  it('should initialize norwegian search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-stopwords-test-no',
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      siNO = thisSi
      getDataStream()
        .pipe(siNO.defaultPipeline({
          stopwords: sw.no
        }))
        .pipe(siNO.add())
        .on('data', function (data) {
        })
        .on('end', function () {
          true.should.be.exactly(true)
          return done()
        })
    })
  })

  it('should be able to return all documents in index', function (done) {
    var results = ['4']
    var i = 0
    siNO.search({
      query: {
        AND: {'*': ['tur']}
      }
    }).on('data', function (data) {
      results.shift().should.be.exactly(data.id)
      i++
    }).on('end', function () {
      i.should.be.exactly(1)
      return done()
    })
  })

  it('"dette" should not give any results since it is blocked by the norwegian stopwords', function (done) {
    var i = 0
    siNO.search({
      query: {
        AND: {'*': ['dette']}
      }
    }).on('data', function (data) {
      i++
    }).on('end', function () {
      i.should.be.exactly(0)
      return done()
    })
  })

  it('should initialize a food search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-stopwords-test-food',
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      siFood = thisSi
      getFoodStream()
        .pipe(siFood.defaultPipeline({
          stopwords: []
        }))
        .pipe(siFood.add())
        .on('data', function (data) {
        })
        .on('end', function () {
          true.should.be.exactly(true)
          return done()
        })
    })
  })

  it('should be able to return results for "fish and chips"', function (done) {
    var results = ['1']
    var i = 0
    siFood.search({
      query: {
        AND: {'*': 'fish and chips'.split(' ')}
      }
    }).on('data', function (data) {
      results.shift().should.be.exactly(data.id)
      i++
    }).on('end', function () {
      i.should.be.exactly(1)
      return done()
    })
  })

  it('should be able to return results for "and"', function (done) {
    var results = ['2', '1']
    var i = 0
    siFood.search({
      query: {
        AND: {'*': ['and']}
      }
    }).on('data', function (data) {
      results.shift().should.be.exactly(data.id)
      i++
    }).on('end', function () {
      i.should.be.exactly(2)
      return done()
    })
  })
})
