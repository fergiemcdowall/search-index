/* global it */
/* global describe */

const JSONStream = require('JSONStream')
const Readable = require('stream').Readable
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const searchindex = require('../../../')
const should = require('should')

var sOne = new Readable({ objectMode: true })
var sTwo = new Readable({ objectMode: true })

var siOne, siTwo

describe('Instantiation: ', function () {
  describe('setting up different indexes with no pollution', function () {
    sOne.push({
      id: 1,
      name: 'The First Doc',
      test: 'this is the first doc'
    })
    sOne.push({
      id: 2,
      name: 'The Second Doc',
      test: 'this is the second doc'
    })
    sOne.push(null)
    sTwo.push({
      id: 3,
      name: 'The Third Doc',
      test: 'this is the third doc'
    })
    sTwo.push({
      id: 4,
      name: 'The Fourth Doc',
      test: 'this is the fourth doc'
    })
    sTwo.push(null)

    it('should initialize the first search index', function (done) {
      searchindex({
        indexPath: sandboxPath + '/si-init-one',
        logLevel: logLevel
      }, function (err, thisSi) {
        if (err) false.should.eql(true)
        siOne = thisSi
        done()
      })
    })

    it('should initialize the first search index', function (done) {
      searchindex({
        indexPath: sandboxPath + '/si-init-two'
      }, function (err, thisSi) {
        if (err) false.should.eql(true)
        siTwo = thisSi
        done()
      })
    })

    it('should index test data into the first index', function (done) {
      sOne.pipe(siOne.defaultPipeline())
        .pipe(siOne.add())
        .on('data', function (data) {})
        .on('end', function () {
          return done()
        })
    })

    it('should index test data into the second index', function (done) {
      sTwo.pipe(siTwo.defaultPipeline())
        .pipe(siTwo.add())
        .on('data', function (data) {})
        .on('end', function () {
          return done()
        })
    })

    it('should be able to search in si-init-one without pollution from si-init-two', function (done) {
      var results = [
        {
          id: 2,
          name: 'The Second Doc',
          test: 'this is the second doc'
        },
        {
          id: 1,
          name: 'The First Doc',
          test: 'this is the first doc'
        }
      ]
      siOne.search({
        query: [{
          AND: {'*': ['*']}
        }]
      }).on('data', function (data) {
        data.document.should.eql(results.shift())
      }).on('end', function () {
        results.length.should.be.exactly(0)
        return done()
      })
    })

    it('should be able to search in si-init-two without pollution from si-init-one', function (done) {
      var results = [
        {
          id: 4,
          name: 'The Fourth Doc',
          test: 'this is the fourth doc'
        },
        {
          id: 3,
          name: 'The Third Doc',
          test: 'this is the third doc'
        }
      ]
      siTwo.search({
        query: [{
          AND: {'*': ['*']}
        }]
      }).on('data', function (data) {
        data.document.should.eql(results.shift())
      }).on('end', function () {
        results.length.should.be.exactly(0)
        return done()
      })
    })
  })
})
