const should = require('should')
const levelup = require('levelup')
const searchIndex = require('../../../')
const sandbox = 'test/sandbox'

var db, si

describe('Making a search-index with a vanilla (leveldown) levelup: ', function () {

  it('should make a new levelup', function (done) {
    levelup(sandbox + '/level-to-be-passed', {valueEncoding: 'json'}, function(err, thisDb) {
      db = thisDb
      done()
    })
  })

  it('should make a new search-index with the existing leveldb', function (done) {
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
      si.search({query:{title: ['cool']}}, function(err, results){
        results.hits[0].document.body.should.equal('this is my doc')
        done()
      })
    })
  })

})
