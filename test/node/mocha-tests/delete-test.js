/* global it */
/* global describe */

const JSONStream = require('JSONStream')
const Readable = require('stream').Readable
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const searchIndex = require('../../../')
const should = require('should')
const sw = require('stopword')

should

describe('deleting: ', function () {
  var si = null

  it('should initialize the search index', function (done) {
    searchIndex({
      indexPath: sandboxPath + '/si-delete-test',
      logLevel: logLevel,
      stopwords: sw.en
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
    var q = {}
    var result = []
    q.query = {
      AND: {'*': ['*']}
    }
    si.search(q).on('data', function (data) {
      result.push(data)
    }).on('end', function (end) {
      result.map(function (item) { return item.id })
        .should.eql(['4', '3', '2', '1'])
      return done()
    })
  })

  it('should be able to delete a document without throwing errorness', function (done) {
    si.del(['2'], function(err) {
      return done()
    })
  })

  it('should be able to return all documents in index, with one document deleted', function (done) {
    var result = []
    var q = {}
    q.query = {
      AND: {'*': ['*']}
    }
    si.search(q).on('data', function (data) {
      result.push(data)
    }).on('end', function (end) {
      result.map(function (item) { return item.id })
        .should.eql(['4', '3', '1'])
      return done()
    })
  })

  it('should index duplicate test data into the index', function (done) {
    var s = new Readable({ objectMode: true })
    s.push({
      id: 1,
      name: 'The First Doc',
      test: 'this is the first doc'
    })
    s.push(null)
    s.pipe(si.defaultPipeline())
      .pipe(si.add())
      .on('data', function (data) {

      })
      .on('end', function () {
        done()
      })
  })

  it('should return 3 docs, since the previously indexed doc is a duplicate', function (done) {
    var result = []
    var q = {}
    q.query = {
      AND: {'*': ['*']}
    }
    si.search(q).on('data', function (data) {
      result.push(data)
    }).on('end', function (end) {
      result.map(function (item) { return item.id })
        .should.eql(['4', '3', '1'])
      return done()
    })
  })
})
