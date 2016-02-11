/* global it */
/* global describe */

var should = require('should')

describe('Filters: ', function () {

  var si

  it('should initialize the search index', function (done) {
    require('../../')({
      indexPath: 'test/sandbox/si-twitter',
      logLevel: 'error'
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
      done()
    })
  })

  it('should index twitter data', function (done) {
    this.timeout(60000)
    var data = require('./twitter-tweets.json')
    var opt = {}
    opt.batchName = 'tweetz'
    opt.fieldOptions = [
      {fieldName: 'tags', filter: true},
      {fieldName: 'user', filter: true}
    ]
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should be able to search in twitter data', function (done) {
    var q = {}
    q.query = {'*': ['search']}
    q.facets = {
      user: {},
      tags: {}
    }
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(8)
      results.totalHits.should.be.exactly(8)
      should.exist(results.facets[0])
      results.facets[0].key.should.be.exactly('user')
      results.facets[0].value[0].key.should.be.exactly('eklem')
      results.facets[0].value[0].value.should.be.exactly(8)
      results.facets[0].value[1].key.should.be.exactly('GoogleforWork')
      ;(results.hits.length > 1).should.be.exactly(true)
      results.hits.length.should.be.exactly(8)
      results.hits[0].id.should.be.exactly('1NsXUW')
      results.hits[1].id.should.be.exactly('TEWP')
      results.hits[2].id.should.be.exactly('4bU7P5')
      done()
    })
  })

  it('should be able to filter by user', function (done) {
    var q = {}
    q.query = {'*': ['search']}
    q.facets = {
      user: {},
      tags: {}
    }
    q.filter = {user: [['GoogleforWork', 'GoogleforWork']]}
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(1)
      results.totalHits.should.be.exactly(1)
      should.exist(results.facets[0])
      results.hits[0].id.should.be.exactly('4EaEkI')
      done()
    })
  })

  it('should be able to filter by tag', function (done) {
    var q = {}
    q.query = {'*': ['search']}
    q.facets = {
      user: {},
      tags: {}
    }
    q.filter = {tags: [['search', 'search']]}
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(5)
      results.totalHits.should.be.exactly(5)
      should.exist(results.facets[0])
      results.hits[0].id.should.be.exactly('TEWP')
      results.hits[1].id.should.be.exactly('4bU7P5')
      results.hits[2].id.should.be.exactly('3swrN')
      done()
    })
  })

  it('should be able to search on tokens that are only found in metadata', function (done) {
    var q = {}
    q.query = {'*': ['eklem']}
    q.facets = {
      user: {},
      tags: {}
    }
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(64)
      results.totalHits.should.be.exactly(64)
      should.exist(results.facets[0])
      results.hits[0].id.should.be.exactly('3gRQbL')
      results.hits[1].id.should.be.exactly('tLl7B')
      done()
    })
  })
})
