/* global it */
/* global describe */

const fs = require('fs')
const assert = require('assert')
const should = require('should')
const sandboxPath = 'test/sandbox'
const searchindex = require('../../../')
const Readable = require('stream').Readable
const JSONStream = require('JSONStream')

describe('Configuration: ', function () {
  it('should accept configuration', function (done) {
    const siPath = sandboxPath + '/si-config'
    searchindex({
      indexPath: siPath
    }, function(err, si) {
      should.exist(si)
      fs.existsSync(siPath).should.be.exactly(true)
      done()
    })
  })

  // it('can be instantiated without an options object', function (done) {
  //   searchindex(function(err, si) {
  //     should.exist(si)
  //     fs.existsSync(si.options.indexPath).should.be.exactly(true)
  //     done()
  //   })
  // })

  it('can be instantiated with an empty options object', function (done) {
    searchindex({}, function(err, si) {
      should.exist(si)
      fs.existsSync(si.options.indexPath).should.be.exactly(true)
      done()
    })
  })

  it('does not leak variables', function () {
    (typeof countDocuments).should.be.exactly('undefined')
    ;(typeof _).should.be.exactly('undefined')
  })

  it('should log when given a logger', function (done) {
    var streamBuffers = require('stream-buffers')
    var myStream = new streamBuffers.WritableStreamBuffer({})
    var log = require('bunyan').createLogger({
      name: 'test-log',
      stream: myStream
    })
    assert.equal(myStream.size(), 0)
    searchindex({
      indexPath: sandboxPath + '/test-log-index',
      log: log,
      logLevel: 'info'
    }, function(err, si) {
      should.exist(si.log)
      const s = new Readable()
      s.push(JSON.stringify({
        id: 1,
        name: 'The First Doc',
        test: 'this is the first doc'
      }))
      s.push(null)  // needed?
      s.pipe(JSONStream.parse())
        .pipe(si.defaultPipeline())
        .pipe(si.add())
        .on('data', function (data) {
          // data.toString().should.be.any('processing doc 1', 'batch replicated')
        })
        .on('end', function () {
          myStream.size().should.be.above(0)
          return done()
        })
    })
  })
})
