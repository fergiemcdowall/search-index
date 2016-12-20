const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const should = require('should')
const sandboxPath = 'test/sandbox'

const getStream = function () {
  var s = new Readable({ objectMode: true })
  s.push({
    id: 1,
    topics: 'barley',
    body: 'Tennents eighty shilling'
  })
  s.push({
    id: 2,
    topics: 'barley, corn',
    body: 'Tennents seventy shilling'
  })
  s.push({
    id: 3,
    topics: 'barley, hops',
    body: 'Deuchars IPA'
  })
  s.push({
    id: 4,
    topics: 'barley, hops',
    body: 'Brew Dog IPA'
  })
  s.push({
    id: 5,
    topics: 'hops',
    body: 'Tennents Lager'
  })
  s.push(null)
  return s
}

describe('init the search index', function() {

  var index

  it('should init without incident', function(done) {
    SearchIndex({
      indexPath: sandboxPath + '/bucket-test'
    }, function (err, thisIndex) {
      if (err) {
        true.should.be.exactly(false)
      } else {
        index = thisIndex
        true.should.be.exactly(true)
      }
      return done()
    })
  })

  it('should index test data into the index', function (done) {
    getStream()
      .pipe(index.defaultPipeline())
      .pipe(index.add())
      .on('data', function (data) {})
      .on('end', function () {
        return done()
      })
  })

  it('should do buckets (without a query object)', function (done) {
    const q = {
      buckets: [{
        field: 'topics',
        gte: 'b',
        lte: 'd',
        set: true
      }]
    }
    index.buckets(q)
      .on('data', function (data) {
        data.value.should.eql(['1', '2', '3', '4'])
      })
      .on('error', function (err) {
        if (err) true.should.eql(false)
      })
      .on('end', function (err) {
        return done()
      })
  })

  it('should do buckets (WITH a query object)', function (done) {
    const q = {
      query: [{
        AND: {
          body: ['tennents']
        }
      }],
      buckets: [{
        field: 'topics',
        gte: 'b',
        lte: 'd',
        set: true
      }]
    }
    index.buckets(q)
      .on('data', function (data) {
        data.value.should.eql(['1', '2'])
      })
      .on('error', function (err) {
        if (err) true.should.eql(false)
      })
      .on('end', function (err) {
        return done()
      })
  })


  it('make a bucket for barley in body:tennents', function (done) {
    const q = {
      query: [{
        AND: {
          body: ['tennents']
        }
      }],
      buckets: [{
        field: 'topics',
        gte: 'barley',
        lte: 'barley',
        set: true
      }]
    }
    index.buckets(q)
      .on('data', function (data) {
        data.value.should.eql(['1', '2'])
      })
      .on('error', function (err) {
        if (err) true.should.eql(false)
      })
      .on('end', function (err) {
        return done()
      })
  })

  it('make 2 buckets in body:tennents', function (done) {
    var results = [
      { field: 'topics',
        gte: 'barley',
        lte: 'barley',
        set: true,
        value: [ '1', '2' ] },
      { field: 'topics', gte: 'hops', lte: 'hops', set: false, value: 1 }
    ]
    const q = {
      query: [{
        AND: {
          body: ['tennents']
        }
      }],
      buckets: [{
        field: 'topics',
        gte: 'barley',
        lte: 'barley',
        set: true
      }, {
        field: 'topics',
        gte: 'hops',
        lte: 'hops',
        set: false
      }]
    }
    index.buckets(q)
      .on('data', function (data) {
        results.shift().should.eql(data)
      })
      .on('error', function (err) {
        if (err) true.should.eql(false)
      })
      .on('end', function (err) {
        results.length.should.be.exactly(0)
        return done()
      })
  })


})
