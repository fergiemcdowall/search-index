/* global it */
/* global describe */

const fs = require('fs')
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const searchindex = require('../../../')
const should = require('should')

describe('Configuration: ', function () {
  it('should accept configuration', function (done) {
    const siPath = sandboxPath + '/si-config'
    searchindex({
      indexPath: siPath,
      logLevel: logLevel
    }, function (err, si) {
      if (err) false.should.eql(true)
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
    searchindex({}, function (err, si) {
      if (err) false.should.eql(true)
      should.exist(si)
      fs.existsSync(si.options.indexPath).should.be.exactly(true)
      done()
    })
  })

  it('does not leak variables', function () {
    (typeof countDocuments).should.be.exactly('undefined')
    ;(typeof _).should.be.exactly('undefined')
  })
})
