const lpad = require('left-pad')
const sandbox = 'test/sandbox/'
const test = require('tape')
const SearchIndex = require('../../../')
const wnum = require('written-number')
const Readable = require('stream').Readable

var si

test('initialize a search index', t => {
  t.plan(1)
  SearchIndex({
    indexPath: sandbox + 'offset-test'
  }, (err, newSi) => {
    t.error(err)
    si = newSi
  })
})

test('concurrently index docs using concurrentAdd', t => {
  t.plan(100)
  for (var i = 1; i <= 100; i++) {
    si.concurrentAdd({}, [{
      id: lpad(i, 3, 0),
      body: 'this is the wonderfully fabulous doc number ' + wnum(i)
    }], err => {
      t.error(err)
    })
  }
})

test('search', t => {
  t.plan(2)
  var i = 0
  si.search('number')
    .on('data', hit => {
      if (i == 0) t.equal(hit.id, '100')
      i++
    })
    .on('end', () => {
      t.equal(i, 20)
    })
    .on('error', e => {
      t.fail(e)
    })
})

test('search with offset', t => {
  t.plan(2)
  var i = 0
  si.search({
    query: [{
      AND: {
        body: ['number']
      }
    }],
    offset: 15
  })
    .on('data', hit => {
      if (i == 0) t.equal(hit.id, '085')
      i++
    })
    .on('end', () => {
      t.equal(i, 20)
    })
    .on('error', e => {
      t.fail(e)
    })
})

test('search with offset', t => {
  t.plan(2)
  var i = 0
  si.search({
    query: [{
      AND: {
        body: ['number']
      }
    }],
    offset: 50,
    pageSize: 7
  })
    .on('data', hit => {
      if (i == 0) t.equal(hit.id, '050')
      i++
    })
    .on('end', () => {
      t.equal(i, 7)
    })
    .on('error', e => {
      t.fail(e)
    })
})

test('search with offset and pageSize defined as strings', t => {
  t.plan(2)
  var i = 0
  si.search({
    query: [{
      AND: {
        body: ['number']
      }
    }],
    offset: "50",
    pageSize: "7"
  })
    .on('data', hit => {
      if (i == 0) t.equal(hit.id, '050')
      i++
    })
    .on('end', () => {
      t.equal(i, 7)
    })
    .on('error', e => {
      t.fail(e)
    })
})
