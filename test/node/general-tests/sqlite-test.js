const levelup = require('levelup')
const sandbox = 'test/sandbox'
const searchIndex = require('../../../')
const should = require('should')
const sqldown = require('sqldown')

var db, si

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
  si.add([{
    title: 'a realllly cool document',
    body: 'this is my doc'
  }], function (err) {
    si.search({
      query: {
        AND: {title: ['cool']}
      }
    }, function(err, results){
      results.hits[0].document.body.should.equal('this is my doc')
      done()
    })
  })
})
