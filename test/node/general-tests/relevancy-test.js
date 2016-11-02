/* global describe */
/* global it */

const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const logLevel = process.env.NODE_ENV || 'error'
const s = new Readable({ objectMode: true })
const sandboxPath = 'test/sandbox'
const should = require('should')

var si

s.push({
  id: '1',
  name: 'Apples',
  description: 'fruit fruit salad'
})
s.push({
  id: '2',
  name: 'Oranges',
  description: 'fruit salad'
})
s.push({
  id: '3',
  name: 'Bananas',
  description: 'fruit fruit fruit fruit salad'
})
s.push({
  id: '4',
  name: 'Grapes',
  description: 'fruit fruit fruit salad'
})
s.push(null)

describe('some simple relevancy tests: ', function () {
  it('should do some simple indexing', function (done) {
    SearchIndex({
      indexPath: sandboxPath + '/relevance-test',
      logLevel: logLevel
    }, function (err, thisSI) {
      if (err) false.should.eql(true)
      si = thisSI
      s.pipe(si.defaultPipeline())
        .pipe(si.add())
        .on('data', function (data) {

        })
        .on('end', function () {
          true.should.be.exactly(true)
          return done()
        })
    })
  })

  it('simple * search, sorted by ID', function (done) {
    var results = []
    si.search({
      query: [{
        AND: {'*': ['*']}
      }]
    }).on('data', function (data) {
      results.push(data)
    }).on('end', function () {
      // console.log(results)
      results.map(function (item) {
        return item.document.id
      }).should.eql(
        [ '4', '3', '2', '1' ])
      return done()
    })
  })

  it('search for salad- "salad" is more relevant when there is less "fruit"', function (done) {
    var results = []
    si.search({
      query: [{
        AND: {'*': ['salad']}
      }]
    }).on('data', function (data) {
      results.push(data)
    }).on('end', function () {
      results.map(function (item) {
        return item.document.id
      }).should.eql(
        [ '2', '1', '4', '3' ])
      return done()
    })
  })

  it('search for fruit- fruit is always the highest occuring term, and therefore results are sorted by id', function (done) {
    var results = []
    si.search({
      query: [{
        AND: {'*': ['fruit']}
      }]
    }).on('data', function (data) {
      results.push(data)
    }).on('end', function () {
      results.map(function (item) {
        return item.document.id
      }).should.eql(
        [ '4', '3', '2', '1' ])
      return done()
    })
  })
})
