/* global it */
/* global describe */

var assert = require('assert')
var _ = require('lodash')
var sandboxPath = 'test/sandbox'
var should = require('should');
var JSONStream = require('JSONStream')

var doc = {
  id: undefined,
  title: 'Mad Science is on the Rise',
  body: 'Mad,  mad things are happening.'
}

describe('Indexing API', function () {

  var si;

  it('should initialize the search index', function (done) {
    require('../../../')({
      indexPath: sandboxPath + '/indexing-test',
      logLevel: 'error'
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
      return done()
    })
  })


  it('should allow indexing a plain object (as opposed to an array) and options object omitted', function (done) {
    var s = new require('stream').Readable()
    s.push(JSON.stringify(_.assign(doc, {id: 1})))
    s.push(null)
    s.pipe(JSONStream.parse())
      .pipe(si.defaultPipeline()).pipe(si.add())
      .on('data', function(data) {
      })
      .on('end', function() {
        si.get(1, function (err, res) {
          assert(_.isEqual(res, doc))
        })
        return done()
      })
  })

  it('should allow indexing with undefined options object', function (done) {
    var s = new require('stream').Readable()
    s.push(JSON.stringify(_.assign(doc, {id: 2})))
    s.push(null)
    s.pipe(JSONStream.parse())
      .pipe(si.defaultPipeline()).pipe(si.add(undefined))
      .on('data', function(data) {
      })
      .on('end', function() {
        si.get(2, function (err, res) {
          assert(_.isEqual(res, doc))
        })
        return done()
      })
  })

  it('should allow indexing with a custom separator', function (done) {
    var s = new require('stream').Readable()
    s.push(JSON.stringify({
      id: 3,
      content: 'Nexion Smart ERP 14.2.1.0\n\nRelease de ejemplo'
    }))
    s.push(null)
    s.pipe(JSONStream.parse())
      .pipe(si.defaultPipeline({
        separator: /[ (\n)]+/
      })).pipe(si.add(undefined))
      .on('data', function(data) {
      })
      .on('end', function() {
        si.search({
          query: [{
            AND: {'*': ['14.2.1.0']}
          }]
        }).on('data', function(data) {
          console.log(data)
        })
        return done()
      })


    // si.add([{
    //   id: 'testing',
    //   content: 'Nexion Smart ERP 14.2.1.0\n\nRelease de ejemplo'
    // }], {
    //   separator: /[ (\n)]+/
    // }, function (err) {
    //   assert.equal(err, null)
    //   si.search({
    //     query: {
    //       AND: {'*': ['14.2.1.0']}
    //     }
    //   }, function (err, res) {
    //     (err === null).should.be.exactly(true)
    //     assert(res.totalHits === 1)
    //     done()
    //   })
    // })
  })
})
