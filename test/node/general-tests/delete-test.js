/* global it */
/* global describe */

const logLevel = 'error'
if (process.env.NODE_ENV === 'TEST') logLevel = 'info'
const should = require('should')
const searchIndex = require('../../../')
const sandboxPath = 'test/sandbox'

describe('deleting: ', function () {
  var si = null

  const data1 = [
    {
      id: 1,
      name: 'The First Doc',
      test: 'this is the first doc'
    },
    {
      id: 2,
      name: 'The Second Doc',
      test: 'this is the second doc'
    },
    {
      id: 3,
      name: 'The Third Doc',
      test: 'this is the third doc doc'
    },
    {
      id: 4,
      name: 'The Fourth Doc',
      test: 'this is the fourth doc'
    }]


  it('should initialize the search index', function (done) {
    searchIndex({
      indexPath: sandboxPath + '/si-delete-test',
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
      done()
    })
  })

  it('should index test data into the index', function (done) {
    si.add(data1, {batchName: 'data1'}, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })      
  })

  it('should be able to return all documents in index', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['*']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(4)
      searchResults.totalHits.should.be.exactly(4)
      done()
    })
  })

  it('should be able to delete a document without throwing errorness', function (done) {
    si.del('2', function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should be able to return all documents in index, with one document deleted', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['*']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(3)
      searchResults.totalHits.should.be.exactly(3)
      searchResults.hits[0].id.should.be.exactly('4')
      searchResults.hits[1].id.should.be.exactly('3')
      searchResults.hits[2].id.should.be.exactly('1')
      done()
    })
  })

  it('should index duplicate test data into the index', function (done) {
    si.add(data1[0], {batchName: 'data2'}, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should return 3 docs, since the previously indexed doc is a duplicate', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['*']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(3)
      searchResults.totalHits.should.be.exactly(3)
      searchResults.hits[0].id.should.be.exactly('4')
      searchResults.hits[1].id.should.be.exactly('3')
      searchResults.hits[2].id.should.be.exactly('1')
      done()
    })
  })

})
