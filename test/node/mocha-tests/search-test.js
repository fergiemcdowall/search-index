const JSONStream = require('JSONStream')
const SearchIndex = require('../../../')
const fs = require('fs')
const logLevel = process.env.LOG_LEVEL || 'error'
const mocha = require('mocha')
const request = require('request')
const sandbox = './test/sandbox/'
const should = require('should')

describe('simple search test', function() {

  var si

  it('should initialize search index', function (done) {
    SearchIndex(
      {indexPath: sandbox + '/si-search-test',
      logLevel: logLevel},
      function (err, thisSi) {
        if (err) false.should.eql(true)
        si = thisSi
        return done()
      })
  })

  it('should post and index a file of data', function (done) {
    fs.createReadStream('./node_modules/reuters-21578-json/data/fullFileStream/justTen.str')
      .pipe(si.feed())
      .on('finish', function () {
        return done()
      })
      .on('error', function (error) {
        true.should.be.exactly(false)
      })
  })

  it('simple search (no search object), sorted by ID', function (done) {
    var results = []
    si.search().on('data', function (data) {
      results.push(data)
    }).on('end', function () {
      results.map(function (item) {
        return item.document.id
      }).should.eql(
        [ '9', '8', '7', '6', '5', '4', '3', '2', '10', '1' ])
      return done()
    })
  })

  it('simple search (empty search object), sorted by ID', function (done) {
    var results = []
    si.search({}).on('data', function (data) {
      results.push(data)
    }).on('end', function () {
      results.map(function (item) {
        return item.document.id
      }).should.eql(
        [ '9', '8', '7', '6', '5', '4', '3', '2', '10', '1' ])
      return done()
    })
  })

  it('knows what fields are available', function (done) {
    var results = []
    si.availableFields().on('data', function (data) {
      results.push(data)
    }).on('end', function () {
      results.map(function (item) {
        return item
      }).should.eql(
        [ '*', 'body', 'date', 'id', 'places', 'title', 'topics' ])
      return done()
    })
  })


})
