/* global it */
/* global describe */

const should = require('should')
const searchindex = require('../../../')

describe('deleting a batch: ', function () {

  this.timeout(10000)
  var si

  it('should initialize the search index', function (done) {
    var sandboxPath = 'test/sandbox'
    searchindex(
      {indexPath: sandboxPath + '/si-reuters-10',
       logLevel: 'error'},
      function (err, thisSi) {
        if (err) false.should.eql(true)
        si = thisSi
        done()
      })
  })

  it('should index one file of test data', function (done) {
    var data = require('../../../node_modules/reuters-21578-json/data/justTen/justTen.json')
    si.add(data, {batchName: 'reuters-000.json'}, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('it should delete documents 1, 3, 5, 7, 10', function (done) {
    si.del(['7', '10', '5', '1', '3'], function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should be able verify that docs are deleted', function (done) {
    si.search({query: {'*': ['*']}}, function (err, result) {
      should.exist(result)
      ;(err === null).should.be.exactly(true)
      result.totalHits.should.be.exactly(5)
      result.hits.length.should.be.exactly(5)
      result.hits[0].id.should.be.exactly('9')
      result.hits[1].id.should.be.exactly('4')
      result.hits[2].id.should.be.exactly('6')
      result.hits[3].id.should.be.exactly('8')
      result.hits[4].id.should.be.exactly('2')
      si.close(function(err){
        done()
      })
    })
  })
})
