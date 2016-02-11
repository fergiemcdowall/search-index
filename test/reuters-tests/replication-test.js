/* global it */
/* global describe */

const sandboxPath = 'test/sandbox'
const searchindex = require('../../')
const should = require('should')
const fs = require('fs')

describe('Replication: ', function () {

  var si

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-reuters-10-replication',
      logLevel: 'error'
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
      done()
    })
  })


  it('should index one file of test data', function (done) {
    this.timeout(5000)
    var data = require('../../node_modules/reuters-21578-json/data/justTen/justTen.json')
    var opt = {}
    opt.batchName = 'reuters'
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should be able to create a snapshot', function (done) {
    this.timeout(5000)
    si.snapShot(function (rs) {
      rs.pipe(fs.createWriteStream(sandboxPath + '/backup.gz'))
        .on('close', function () {
          (true).should.be.exactly(true)
          done()
        })
        .on('error', function (err) {
          (err === null).should.be.exactly(true)
        })
    })
  })

  it('should empty the index', function (done) {
    si.flush(function (err) {
      // Is this a bug in levelUP? Should undefined be null?
      (err === undefined).should.be.exactly(true)
      done()
    })
  })

  it('should be able to display information about the index (index is empty)', function (done) {
    si.tellMeAboutMySearchIndex(function (err, result) {
      (err === undefined).should.be.exactly(false)
      result.totalDocs.should.be.exactly(0)
      done()
    })
  })

  it('should be able to refeed from a snapshot', function (done) {
    this.timeout(5000)
    si.replicate(fs.createReadStream(sandboxPath + '/backup.gz'), function (err) {
      (err === undefined).should.be.exactly(true)
      done()
    })
  })

  it('should be able to display information about the index (index has 10 docs)', function (done) {
    si.tellMeAboutMySearchIndex(function (err, result) {
      (err === null).should.be.exactly(true)
      should.exist(result)
      result.totalDocs.should.be.exactly(10)
      done()
    })
  })
})
