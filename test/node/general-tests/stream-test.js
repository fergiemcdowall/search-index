
const fs = require('fs')
const should = require('should')
const SearchIndex = require('../../../')
const test = require('tape')
const JSONstream = require('JSONstream')

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
    .pipe(JSONstream.parse())
    .pipe(si.createWriteStream())
    .on('data', function(data) {

    }).on('end', function() {
      done()
    })
})


it('index should be searchable', function (done) {
  si.search({
    query: {
      AND: [{'*': ['*']}]
    }
  }, function (err, results) {
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '9', '8', '7', '6', '5', '4', '3', '2', '10', '1' ]
    )
    done()
  })
})


