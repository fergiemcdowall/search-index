const sandbox = 'test/sandbox/'
const test = require('tape')
const SearchIndex = require('../../../')
const num = require('written-number')
const Readable = require('stream').Readable
const batchSize = 100

var si

test('initialize a search index', t => {
  t.plan(1)
  SearchIndex({
    indexPath: sandbox + 'reindex-test'
  }, (err, newSi) => {
    t.error(err)
    si = newSi
  })
})

test('make an index', t => {
  t.plan(1)
  var s = new Readable({ objectMode: true })
  for (var i = 1; i <= batchSize; i++) {
    s.push({
      id: i,
      tokens: 'this is the amazing doc number ' + num(i)
    })
  }
  s.push(null)
  s.pipe(si.feed({ objectMode: true }))
    .on('finish', function() {
      t.pass('finished')
    })
    .on('error', function(err) {
      t.error(err)
    })
})

test('search', t => {
  t.plan(20)
  si.search()
    .on('data', function() {
      t.pass('got a result')
    })
})

test('reindex a document', t => {
  t.plan(1)
  var s = new Readable({ objectMode: true })
  s.push({
    id: 5,
    tokens: 'this is the all new amazing doc number five'
  })
  s.push(null)
  s.pipe(si.feed({ objectMode: true }))
    .on('finish', function() {
      t.pass('finished')
    })
    .on('error', function(err) {
      t.error(err)
    })
})

test('search', t => {
  t.plan(1)
  si.search({
    query: [{
      AND: {id: ['5']}
    }]
  })
    .on('data', function(d) {
      t.looseEquals(d.document, {
        id: 5,
        tokens: 'this is the all new amazing doc number five'
      })
    })
})
