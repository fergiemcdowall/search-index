/* global describe */
/* global it */

const Readable = require('stream').Readable
const levelup = require('levelup')
const logLevel = process.env.NODE_ENV || 'error'
const sandbox = 'test/sandbox'
const searchIndex = require('../../../')
const should = require('should')

var db, si

describe('Making a search-index with a vanilla (leveldown) levelup: ', function () {
  it('should make a new levelup', function (done) {
    levelup(sandbox + '/level-to-be-passed', {valueEncoding: 'json'}, function (err, thisDb) {
      ;(!err).should.be.exactly(true)
      db = thisDb
      done()
    })
  })

  it('should make a new search-index with the existing leveldb', function (done) {
    searchIndex({
      indexes: db,
      logLevel: logLevel
    }, function (err, thisSi) {
      should.exist(thisSi)
      ;(!err).should.be.exactly(true)
      si = thisSi
      return done()
    })
  })

  it('should be able to index and search as normal', function (done) {
    const s = new Readable({ objectMode: true })
    s.push({
      title: 'a realllly cool document',
      body: 'this is my doc'
    })
    s.push(null)
    s.pipe(si.defaultPipeline())
      .pipe(si.add())
      .on('data', function (data) {

      })
      .on('end', function () {
        si.search({
          query: [{
            AND: {'*': ['now sadly defunct']}
          }]
        }).on('data', function (data) {
          JSON.parse(data).document.body.should.equal('this is my doc')
          return done()
        })
        return done()
      })
  })
})
