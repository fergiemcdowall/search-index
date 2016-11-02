/* global describe */
/* global it */

const JSONStream = require('JSONStream')
const Readable = require('stream').Readable
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const searchIndex = require('../../../')
const should = require('should')

var si

should  // appease standard

describe('.get-ting: ', function () {
  it('should initialize the search index', function (done) {
    searchIndex({
      indexPath: sandboxPath + '/si-get-test',
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
      done()
    })
  })

  it('should index test data into the index', function (done) {
    var s = new Readable({ objectMode: true })
    s.push({
      id: 1,
      name: 'The First Doc',
      test: 'this is the first doc'
    })
    s.push({
      id: 2,
      name: 'The Second Doc',
      test: 'this is the second doc'
    })
    s.push({
      id: 3,
      name: 'The Third Doc',
      test: 'this is the third doc doc'
    })
    s.push({
      id: 4,
      name: 'The Fourth Doc',
      test: 'this is the fourth doc'
    })
    s.push(null)
    s.pipe(si.defaultPipeline())
      .pipe(si.add())
      .on('data', function (data) {}).on('end', function () {
        done()
      })
  })

  it('should be able to return all documents in index', function (done) {
    var results = [
      {
        id: 3,
        name: 'The Third Doc',
        test: 'this is the third doc doc'
      }
    ]
    si.get(['3']).on('data', function (data) {
      data.should.eql(results.shift())
    }).on('end', function (end) {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('should be able to return all documents in index', function (done) {
    var results = [
      {
        id: 3,
        name: 'The Third Doc',
        test: 'this is the third doc doc'
      },
      {
        id: 1,
        name: 'The First Doc',
        test: 'this is the first doc'
      },
      {
        id: 4,
        name: 'The Fourth Doc',
        test: 'this is the fourth doc'
      }
    ]
    si.get(['3', '1', '4']).on('data', function (data) {
      data.should.eql(results.shift())
    }).on('end', function (end) {
      results.length.should.be.exactly(0)
      return done()
    })
  })
})
