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
    indexPath: sandbox + 'wildcard-test'
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
    wildcard: false
  }))
    .on('finish', function() {
      t.pass('finished')
    })
    .on('error', function(err) {
      t.error(err)
    })
})

test('wildcard search shouldnt work', t => {
  t.plan(1)
  si.search()
    .on('data', function() {})
    .on('end', function() {
      t.pass('no results here')
    })
    .on('error', function(err) {
      t.error(err)
    })
})

test('search for a word should work', t => {
  t.plan(11)
  si.search('amazing')
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

test('reindex for wildcards', t => {
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
    wildcard: true
  }))
    .on('finish', function() {
      t.pass('finished')
    })
    .on('error', function(err) {
      t.error(err)
    })
})

test('wildcard search should now work', t => {
  t.plan(11)
  si.search()
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
