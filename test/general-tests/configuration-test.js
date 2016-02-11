/* global it */
/* global describe */

var fs = require('fs')
var assert = require('assert')
var should = require('should')
var sandboxPath = 'test/sandbox'
var searchindex = require('../../')

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
      si.add({
        id: 1,
        name: 'The First Doc',
        test: 'this is the first doc'
      }, function (err) {
        should(err).be.undefined
        myStream.size().should.be.above(0)
        done()
      })
    })
  })
})
