/* global it */
/* global describe */

const JSONStream = require('JSONStream')
const Readable = require('stream').Readable
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const searchIndex = require('../../../')
const should = require('should')

should

const getDocStream = function () {
  var s = new Readable()
  s.push(JSON.stringify({
    id: 1,
    name: 'The First Doc',
    test: 'this is the first doc'
  }))
  s.push(JSON.stringify({
    id: 2,
    name: 'The Second Doc',
    test: 'this is the second doc'
  }))
  s.push(JSON.stringify({
    id: 3,
    name: 'The Third Doc',
    test: 'this is the third doc doc'
  }))
  s.push(JSON.stringify({
    id: 4,
    name: 'The Fourth Doc',
    test: 'this is the fourth doc'
  }))
  s.push(null)
  return s
}

describe('testing case: ', function () {
  var si, si2

  it('should initialize the search index', function (done) {
    searchIndex({
      indexPath: sandboxPath + '/si-case-test',
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
      done()
    })
  })

  it('should index test data into the index', function (done) {
    getDocStream().pipe(JSONStream.parse())
      .pipe(si.defaultPipeline({
        preserveCase: true
      }))
      .pipe(si.add())
      .on('data', function (data) {}).on('end', function () {
        done()
      })
  })

  it('Since preserve case is true, "name:third" should not return results', function (done) {
    var i = 0
    var q = {}
    q.query = {
      AND: {'name': ['third']}
    }
    si.search(q).on('data', function (data) {
      i++
    }).on('end', function (end) {
      i.should.be.exactly(0)
      return done()
    })
  })

  it('...but "name:Third" _should_ return results', function (done) {
    var i = 0
    var q = {}
    q.query = {
      AND: {'name': ['Third']}
    }
    si.search(q).on('data', function (data) {
      i++
    }).on('end', function (end) {
      i.should.be.exactly(1)
      return done()
    })
  })

  it('should initialize the search index', function (done) {
    searchIndex({
      indexPath: sandboxPath + '/si-case-test-2',
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si2 = thisSi
      done()
    })
  })

  it('should index test data into the index', function (done) {
    getDocStream().pipe(JSONStream.parse())
      .pipe(si2.defaultPipeline({
        preserveCase: false
      }))
      .pipe(si2.add())
      .on('data', function (data) {}).on('end', function () {
        done()
      })
  })

  it('Since preserve case is false, "name:third" _should_ return results', function (done) {
    var i = 0
    var q = {}
    q.query = {
      AND: {'name': ['third']}
    }
    si2.search(q).on('data', function (data) {
      i++
    }).on('end', function (end) {
      i.should.be.exactly(1)
      return done()
    })
  })

  it('...but "name:Third" _should not_ return results', function (done) {
    var i = 0
    var q = {}
    q.query = {
      AND: {'name': ['Third']}
    }
    si2.search(q).on('data', function (data) {
      i++
    }).on('end', function (end) {
      i.should.be.exactly(0)
      return done()
    })
  })
})
