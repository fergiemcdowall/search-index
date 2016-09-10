const levelup = require('levelup')
const sandbox = 'test/sandbox'
const searchIndex = require('../../../')
const should = require('should')
const sqldown = require('sqldown')
const JSONStream = require('JSONStream')

var db, si


// NOTE: sqldown will look for a backend that isnt mentioned here you
// need to do "npm install --save sqlite3" or similar, see
// https://www.npmjs.com/package/sqldown for details


it('should make a new levelup with sqlite', function (done) {
  levelup(sandbox + '/level-sqlite', {
    valueEncoding: 'json',
    db: sqldown
  }, function(err, thisDb) {
    db = thisDb
    done()
  })
})

it('should make a new search-index with the sqlite backed leveldb', function (done) {
  searchIndex({
    indexes: db
  }, function(err, thisSi) {
    si = thisSi
    done()
  })
})

it('should be able to index and search as normal', function (done) { 
    var i = 0
    const s = new require('stream').Readable()
    s.push(JSON.stringify({
      title: 'a realllly cool document',
      body: 'this is my doc'
    }))
    s.push(null)
    s.pipe(JSONStream.parse())
      .pipe(si.defaultPipeline())
      .pipe(si.add())
      .on('data', function(data) {
        i++
      })
      .on('end', function() {
        i.should.be.exactly(2)
        si.search({
          query: [{
            AND: {'*': ['cool']}
          }]
        }).on('data', function(data) {
          JSON.parse(data).document.body.should.equal('this is my doc')
          return done()
        })
      })
})
