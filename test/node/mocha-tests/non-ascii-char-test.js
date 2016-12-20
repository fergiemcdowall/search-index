/* global it */
/* global describe */

const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const logLevel = process.env.NODE_ENV || 'error'
const s = new Readable({ objectMode: true })
const sandboxPath = 'test/sandbox'
const should = require('should')

var si

describe('Indexing and searching non-ascii characters: ', function () {
  s.push({
    id: 1,
    names: 'ståle synnøve Kjærsti',
    test: 'this doc should give hits smør for peaches peaches all of the tokens in the names field'
  })
  s.push({
    id: 2,
    names: 'Gerät Grünnerløkka',
    test: 'everything in names doc field smør should be searchable searchable searchable'
  })
  s.push(null)

  it('should initialize the first search index', function (done) {
    SearchIndex({
      indexPath: sandboxPath + '/si-non-ascii',
      logLevel: logLevel
    }, function (err, thisSi) {
      should.exist(thisSi)
      ;(!err).should.be.exactly(true)
      si = thisSi
      return done()
    })
  })

  it('should index test data', function (done) {
    s.pipe(si.defaultPipeline())
      .pipe(si.add())
      .on('data', function (data) {

      })
      .on('end', function () {
        true.should.be.exactly(true)
        return done()
      })
  })

  it('should be able to search in test data', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['ståle', 'synnøve', 'kjærsti']}
    }
    si.search(q)
      .on('data', function (data) {
        data.document.should.eql({
          id: 1,
          names: 'ståle synnøve Kjærsti',
          test: 'this doc should give hits smør for peaches peaches all of the tokens in the names field'
        })
      })
      .on('end', function () {
        return done()
      })
  })

  it('should be able to search in test data', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['gerät', 'grünnerløkka']}
    }
    si.search(q)
      .on('data', function (data) {
        data.document.should.eql({
          id: 2,
          names: 'Gerät Grünnerløkka',
          test: 'everything in names doc field smør should be searchable searchable searchable'
        })
      })
      .on('end', function () {
        return done()
      })
  })
})
