/* global it */
/* global describe */

const fs = require('fs')
const searchindex = require('../../../')
const should = require('should')

describe('Indexing Reuters without IDs: ', function () {

  var data = require('../../../node_modules/reuters-21578-json/data/noIDs/reuters-000.json')
  var sandboxPath = 'test/sandbox'
  var si

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-reuters-no-ids',
      logLevel: 'error'
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
      done()
    })
  })

  it('should find the data and set up a sandbox', function (done) {
    data.length.should.be.exactly(1000)
    should.not.exist(data.id)
    try {
      var stats = fs.lstatSync(sandboxPath)
      stats.isDirectory().should.be.exactly(true)
    } catch (e) {
      console.log(e)
      true.should.be.exactly(false)
    }
    done()
  })

  it('should index one file of test data that doesnt contain IDs', function (done) {
    this.timeout(60000)
    var opt = {}
    opt.batchName = 'reuters no ids'
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should verify indexing', function (done) {
    this.timeout(10000)
    si.tellMeAboutMySearchIndex(function (err, info) {
      (err === null).should.be.exactly(true)
      should.exist(info)
      ;(info.totalDocs).should.be.exactly(1000)
      done()
    })
  })

  it('verifies recalibration', function (done) {
    si.options.indexes.get('TF￮*￮1987￮￮', function (err, value) {
      (err === null).should.be.exactly(true)
      value.length.should.be.exactly(1000)
      done()
    })
  })

  it('should search on all fields and get results', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['usa']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.above(1)
      searchResults.hits.length.should.be.exactly(100)
      done()
    })
  })

  it('should be able to handle multiword searches', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['reuter', '1987']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(100)
      searchResults.totalHits.should.be.exactly(922)
      done()
    })
  })
})
