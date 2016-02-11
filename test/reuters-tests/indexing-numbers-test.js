/* global it */
/* global describe */

const should = require('should')
const searchindex = require('../../')

describe('Indexing numeric fields, Reuters: ', function () {

  var si

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: 'test/sandbox/si-reuters-10-2',
      logLevel: 'error'
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
      done()
    })
  })


  it('should index one file of test data', function (done) {
    this.timeout(20000)
    var data = require('../../node_modules/reuters-21578-json/data/justTen/justTen.json')
    var opt = {}
    opt.batchName = 'reuters'
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('verifies calibration of number after batch is indexed', function (done) {
    si.options.indexes.get('TF￮randomNumber￮2749￮￮', function (err, value) {
      (err === null).should.be.exactly(true)
      value.length.should.be.exactly(1)
      done()
    })
  })

  it('should verify indexing', function (done) {
    si.tellMeAboutMySearchIndex(function (err, info) {
      (err === null).should.be.exactly(true)
      should.exist(info)
      ;(info.totalDocs).should.be.exactly(10)
      done()
    })
  })

  it('should be able to search number fields in indexed datas', function (done) {
    var q = {}
    q.query = {randomNumber: [2749]}
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(1)
      results.hits[0].id.should.be.exactly('9')
      done()
    })
  })
})
