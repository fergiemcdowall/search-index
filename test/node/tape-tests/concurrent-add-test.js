const sandbox = 'test/sandbox/'
const test = require('tape')
const SearchIndex = require('../../../')
const num = require('written-number')
const Readable = require('stream').Readable

var si, si2

test('initialize a search index', t => {
  t.plan(1)
  SearchIndex({
    indexPath: sandbox + 'concurrency-test'
  }, (err, newSi) => {
    t.error(err)
    si = newSi
  })
})

test('concurrently index docs using concurrentAdd', t => {
  t.plan(100)
  for (var i = 1; i <= 100; i++) {
    var writtenNum = num(i)
    si.concurrentAdd({}, [{
      id: writtenNum,
      body: 'this is the wonderfully fabulous doc number ' + writtenNum
    }], err => {
      t.error(err)
    })
  }
})

test('countDocs', t => {
  t.plan(2)
  si.countDocs((err, count) => {
    t.error(err)
    t.equal(count, 100)
  })
})

test('check key length', t => {
  t.plan(2)
  si.options.indexes.get('TF￮id￮ninety', (err, val) => {
    t.error(err)
    t.equal(val.length, 10)
  })
})

test('check key length', t => {
  t.plan(2)
  si.options.indexes.get('TF￮id￮*', (err, val) => {
    t.error(err)
    t.equal(val.length, 100)
  })
})

test('check key length', t => {
  t.plan(2)
  si.options.indexes.get('DF￮id￮*', (err, val) => {
    t.error(err)
    t.equal(val.length, 100)
  })
})

test('will another add work?', t => {
  t.plan(1)
  si.concurrentAdd({}, [{
    id: 'boom',
    body: 'this is the wonderfully fabulous last doc to be added'
  }], err => {
    t.error(err)
  }), 3000
})

test('check key length', t => {
  t.plan(2)
  si.options.indexes.get('DF￮id￮*', (err, val) => {
    t.error(err)
    t.equal(val.length, 101)
  })
})
