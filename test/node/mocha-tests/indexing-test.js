/* global it */
/* global describe */

const JSONStream = require('JSONStream')
const Readable = require('stream').Readable
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const should = require('should')

describe('Indexing API', function () {
  var si

  it('should initialize the search index', function (done) {
    require('../../../')({
      indexPath: sandboxPath + '/indexing-test',
      logLevel: logLevel
    }, function (err, thisSi) {
      should.exist(thisSi)
      if (err) false.should.eql(true)
      si = thisSi
      return done()
    })
  })

  it('should allow indexing a plain object (as opposed to an array) and options object omitted', function (done) {
    var s = new Readable({ objectMode: true })
    var doc = {
      id: '1',
      title: 'Mad Science is on the Rise',
      body: 'Mad,  mad things are happening.'
    }
    s.push(doc)
    s.push(null)
    s.pipe(si.defaultPipeline()).pipe(si.add())
      .on('data', function (data) {})
      .on('end', function () {
        si.get(['1']).on('data', function (data) {
          data.should.eql(doc)
        })
        .on('end', function () {
          return done()
        })
      })
  })

  it('should allow indexing with undefined options object', function (done) {
    var s = new Readable({ objectMode: true })
    var doc = {
      id: '2',
      title: 'Mad Science is on the Rise',
      body: 'Mad,  mad things are happening.'
    }
    s.push(doc)
    s.push(null)
    s.pipe(si.defaultPipeline())
      .pipe(si.add(undefined))
      .on('data', function (data) {})
      .on('end', function () {
        si.get(['2']).on('data', function (data) {
          data.should.eql(doc)
        })
        .on('end', function () {
          return done()
        })
      })
  })

  it('should allow indexing with a custom separator', function (done) {
    var doc = {
      id: '3',
      content: 'Nexion Smart ERP 14.2.1.0\n\nRelease de ejemplo'
    }
    var s = new Readable({ objectMode: true })
    var i = 0
    s.push(doc)
    s.push(null)
    s.pipe(si.defaultPipeline({
      separator: /\s+/
    })).pipe(si.add())
      .on('data', function (data) {})
      .on('end', function () {
        si.search({
          query: [{
            AND: {'*': ['14.2.1.0']}
          }]
        }).on('data', function (data) {
          i++
          data.document.should.eql(doc)
        }).on('end', function () {
          i.should.be.exactly(1)
          return done()
        })
      })
  })

  it('should allow indexing of a pre-tokenised field', function (done) {
    var doc = {
      id: '3',
      content: ['Nexion', 'Smart', 'ERP', '14.2.1.0', 'Release', 'de', 'ejemplo']
    }
    var s = new Readable({ objectMode: true })
    var i = 0
    s.push(doc)
    s.push(null)
    s.pipe(si.defaultPipeline({
      separator: /\s+/
    })).pipe(si.add())
      .on('data', function (data) {})
      .on('end', function () {
        si.search({
          query: [{
            AND: {'*': ['14.2.1.0']}
          }]
        }).on('data', function (data) {
          i++
          data.document.should.eql(doc)
        }).on('end', function () {
          i.should.be.exactly(1)
          return done()
        })
      })
  })

  it('can count docs', function (done) {
    si.countDocs(function (err, docCount) {
      if (err) false.should.eql(true)
      docCount.should.be.exactly(3)
      return done()
    })
  })
})
