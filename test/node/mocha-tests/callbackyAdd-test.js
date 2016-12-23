/* global describe */
/* global it */

const SearchIndex = require('../../../')
const logLevel = process.env.NODE_ENV || 'error'
const should = require('should')

describe('Callbacky Add', function () {
  const data = [
    {
      id: 'one',
      text: 'the first doc'
    },
    {
      id: 'two',
      text: 'the second doc'
    },
    {
      id: 'three',
      text: 'the third doc'
    },
    {
      id: 'four',
      text: 'the fourth doc'
    }
  ]

  var si

  it('Should initialize the search index', function (done) {
    SearchIndex({
      indexPath: 'test/sandbox/callbacky-add',
      logLevel: logLevel
    }, function (err, thisSi) {
      should.exist(thisSi)
      if (err) false.should.eql(true)
      si = thisSi
      return done()
    })
  })

  it('Should allow indexing via callbacky api', function (done) {
    si.concurrentAdd({}, data, function (err) {
      if (err) false.should.eql(true)
      return done()
    })
  })

  it('Indexing actually worked', function (done) {
    var results = [ 'two', 'three', 'one', 'four' ]
    si.search({
      AND: {'*': ['*']}
    }).on('data', function (data) {
      data.document.id.should.be.exactly(results.shift())
    }).on('end', function () {
      true.should.eql(true)
      return done()
    })
  })
})
