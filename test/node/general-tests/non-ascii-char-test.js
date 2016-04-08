/* global it */
/* global describe */

var should = require('should')
var sandboxPath = 'test/sandbox'
var searchindex = require('../../../')
var si

describe('Indexing and searching non-ascii characters: ', function () {
  var data = [
    {
      id: 1,
      names: 'ståle synnøve Kjærsti',
      test: 'this doc should give hits smør for peaches peaches all of the tokens in the names field'
    },
    {
      id: 2,
      names: 'Gerät Grünnerløkka',
      test: 'everything in names doc field smør should be searchable searchable searchable'
    }]

  it('should initialize the first search index', function (done) {
    searchindex(
      {indexPath: sandboxPath + '/si-non-ascii',
       logLevel: 'error'},
      function (err, thisSi) {
        if (err) false.should.eql(true)
        si = thisSi
        done()
      })
  })

  it('should index test data', function (done) {
    si.add(data, {}, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should be able to search in test data', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['ståle', 'synnøve', 'kjærsti']}
    }
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(1)
      results.totalHits.should.be.exactly(1)
      results.hits[0].id.should.be.exactly('1')
      done()
    })
  })

  it('should be able to search in test data', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['gerät', 'grünnerløkka']}
    }
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(1)
      results.totalHits.should.be.exactly(1)
      results.hits[0].id.should.be.exactly('2')
      done()
    })
  })
})
