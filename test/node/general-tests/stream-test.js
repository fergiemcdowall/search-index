
const fs = require('fs')
const should = require('should')
const SearchIndex = require('../../../')
const test = require('tape')
const JSONStream = require('JSONStream')

var si

it('init indexer', function (done) {
  SearchIndex({
    indexPath: 'test/sandbox/si-stream'
  }, function (err, thisSi) {
    ;(err === null).should.be.exactly(true)
    si = thisSi
    done()
  })
})


it('test stream file', function (done) {
  this.timeout(5000);
  var i = 0
  const filePath = './node_modules/reuters-21578-json/data/fullFileStream/justTen.str'
  require('readline').createInterface({
    input: fs.createReadStream(filePath)
  })
    .on('line', function(line) {
      i++
    })
    .on('close', function() {
      i.should.be.exactly(10)
      done()
    })
})


it('stream file to search-index', function (done) {
  this.timeout(5000);
  const filePath = './node_modules/reuters-21578-json/data/fullFileStream/justTen.str'
  fs.createReadStream(filePath)
    .pipe(JSONStream.parse())
    .pipe(si.defaultPipeline())
    .pipe(si.add())
    .on('data', function(data) {

    }).on('end', function() {
      done()
    })
})


it('index should be searchable', function (done) {
  var i = 0
  var results = [ '9', '8', '7', '6', '5', '4', '3', '2', '10', '1' ]
  si.search({
    query: {
      AND: {'*': ['*']}
    }
  }).on('data', function(data) {
    i++
    data = JSON.parse(data)
    data.id.should.be.exactly(results.shift())
  }).on('end', function() {
    i.should.be.exactly(10)
    return done()
  })
})


