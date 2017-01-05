/* global it */
/* global describe */

const Readable = require('stream').Readable
const logLevel = process.env.NODE_ENV || 'error'
const s = new Readable({ objectMode: true })
const sandboxPath = 'test/sandbox'
const should = require('should')
const stopwords = require('stopword').en

s.push({
  id: 'a',
  title: 'The Beatles',
  content: 'The Beatles were an English rock band, formed in Liverpool in 1960. Beatles from Liverpool',
  year: ['1960', '1961', '1962']
})
s.push({
  id: 'b',
  title: 'The Rolling Stones',
  content: 'The Rolling Stones are an English rock band formed in London in 1962.',
  year: ['1962', '1963', '1964']
})
s.push({
  id: 'c',
  title: 'Pink Floyd',
  ignoreThis: 'bob loblaws law blog',
  content: 'Pink Floyd were an English rock band formed in London London London. Pink Floyd achieved international acclaim with their progressive and psychedelic music.',
  year: ['1963', '1964', '1965']
})
s.push(null)

describe('Indexing API', function () { // jshint ignore:line
  it('should do some simple indexing', function (done) {
    var searchindex = require('../../../')
    searchindex({
      indexPath: sandboxPath + '/indexing-test-2',
      logLevel: logLevel,
      stopwords: stopwords
    }, function (err, si) {
      should.exist(si)
      if (err) false.should.eql(true)
      var i = 0
      s.pipe(si.defaultPipeline())
        .pipe(si.add())
        .on('data', function (data) {})
        .on('end', function () {
          si.dbReadStream()
            .on('data', function (data) {
              i++
            })
            .on('close', function (data) {
              i.should.be.exactly(167)
              return done()
            })
        })
    })
  })
})
