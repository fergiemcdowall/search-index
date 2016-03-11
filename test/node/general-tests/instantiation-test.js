/* global it */
/* global describe */

var should = require('should')
var sandboxPath = 'test/sandbox'
var searchindex = require('../../../')

describe('Instantiation: ', function () {
  describe('setting up different indexes with no pollution', function () {

    var siOne, siTwo

    var data1 = [
      {
        id: 1,
        name: 'The First Doc',
        test: 'this is the first doc'
      },
      {
        id: 2,
        name: 'The Second Doc',
        test: 'this is the second doc'
      }]

    var data2 = [
      {
        id: 3,
        name: 'The Third Doc',
        test: 'this is the third doc'
      },
      {
        id: 4,
        name: 'The Fourth Doc',
        test: 'this is the fourth doc'
      }]

    it('should initialize the first search index', function (done) {
      searchindex({
        indexPath: sandboxPath + '/si-init-one'
      }, function (err, thisSi) {
        if (err) false.should.eql(true)
        siOne = thisSi
        done()
      })
    })

    it('should initialize the first search index', function (done) {
      searchindex({
        indexPath: sandboxPath + '/si-init-two'
      }, function (err, thisSi) {
        if (err) false.should.eql(true)
        siTwo = thisSi
        done()
      })
    })


    it('should index test data into the first index', function (done) {
      siOne.add(data1, {}, function (err) {
        (err === null).should.be.exactly(true)
        done()
      })
    })

    it('should index test data into the second index', function (done) {
      siTwo.add(data2, {}, function (err) {
        (err === null).should.be.exactly(true)
        done()
      })
    })

    it('should be able to search in si-init-one without pollution from si-init-two', function (done) {
      var q = {}
      q.query = {'*': ['*']}
      siOne.search(q, function (err, results) {
        should.exist(results)
        ;(err === null).should.be.exactly(true)
        results.hits.length.should.be.exactly(2)
        results.totalHits.should.be.exactly(2)
        results.hits[0].id.should.be.exactly('2')
        results.hits[1].id.should.be.exactly('1')
        done()
      })
    })

    it('should be able to search in si-init-two without pollution from si-init-one', function (done) {
      var q = {}
      q.query = {'*': ['*']}
      siTwo.search(q, function (err, results) {
        should.exist(results)
        ;(err === null).should.be.exactly(true)
        results.hits.length.should.be.exactly(2)
        results.totalHits.should.be.exactly(2)
        results.hits[0].id.should.be.exactly('4')
        results.hits[1].id.should.be.exactly('3')
        done()
      })
    })
  })
})
