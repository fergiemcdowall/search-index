const test = require('tape')
const SearchIndex = require('../../../')

var index, ids

test('init search index', function (t) {
  t.plan(1)
  SearchIndex({
    indexPath: 'test/sandbox/376-test'
  }, function(err, idx) {
    t.error(err)
    index = idx
  })
})


test('add 1000 identical docs using concurrent add', function (t) {
  t.plan(1)
  var batch = []
  for (let i = 0; i < 1000; i++) {
    batch.push({
      main: 'test.assembly'
    })
  }
  index.concurrentAdd({}, batch, function (err) {
    t.error(err)
  })
})


test('harvest ids', function (t) {
  t.plan(1)
  ids = []
  index
    .search({
      pageSize: 1000
    })
    .on('data', function(doc) {
      ids.push(doc.id)
    }).on('end', function() {
      t.is(ids.length, 1000)
    })
})

test('delete docs', function (t) {
  t.plan(1)
  index.del(ids, function(err) {
    t.error(err)
  })
})

test('search should return no docs', function (t) {
  t.plan(1)
  var i = 0
  index
    .search()
    .on('data', function (doc) {
      i++
    }).on('end', function () {
      t.is(i, 0)
    })
})
