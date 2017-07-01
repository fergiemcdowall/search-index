const sandbox = 'test/sandbox/'
const test = require('tape')
const SearchIndex = require('../../../')
const num = require('written-number')
const Readable = require('stream').Readable
const batchSize = 10

var si

test('initialize a search index', t => {
  t.plan(1)
  SearchIndex({
    indexPath: sandbox + 'compositefield-test'
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
  s.pipe(si.feed({
    objectMode: true,
    compositeField: false
  }))
    .on('finish', function() {
      t.pass('finished')
    })
    .on('error', function(err) {
      t.error(err)
    })
})

test('compositeField search shouldnt work', t => {
  t.plan(1)
  si.search({
    query: [{
      AND: {'*': ['*']}
    }]
  })
    .on('data', function() {})
    .on('end', function() {
      t.pass('no results here')
    })
    .on('error', function(err) {
      t.error(err)
    })
})

test('search on specified fields should work', t => {
  t.plan(11)
  si.search({
    query: [{
      AND: {'tokens': ['*']}
    }]
  })
    .on('data', function(d) {
      t.pass('got a result')
    })
    .on('end', function() {
      t.pass('ended')
    })
    .on('error', function(err) {
      t.error(err)
    })
})

test('reindex for compositeFields', t => {
  t.plan(1)
  var s = new Readable({ objectMode: true })
  for (var i = 1; i <= batchSize; i++) {
    s.push({
      id: i,
      tokens: 'this is the amazing doc number ' + num(i)
    })
  }
  s.push(null)
  s.pipe(si.feed({
    objectMode: true,
    compositeField: true
  }))
    .on('finish', function() {
      t.pass('finished')
    })
    .on('error', function(err) {
      t.error(err)
    })
})

test('compositeField search should now work', t => {
  t.plan(11)
  si.search({
    query: [{
      AND: {'*': ['*']}
    }]
  })
    .on('data', function() {
      t.pass('git a doc')
    })
    .on('end', function() {
      t.pass('end')
    })
    .on('error', function(err) {
      t.error(err)
    })
})

test('deleting a doc', t => {
  t.plan(1)
  si.del(['3'], function (err) {
    t.error(err)
  })
})
