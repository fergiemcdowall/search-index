const test = require('tape')
const SearchIndex = require('../../../')
const batchSize = 10

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
  t.plan(batchSize)
  var batch = []
  for (var i = 0; i < batchSize; i++) {
    index.concurrentAdd({}, [{
      id: i + 'a',
      main: 'test.assembly'
    }], function (err) {
      t.error(err)
    })
  }
})


test('harvest ids', function (t) {
  t.plan(1)
  ids = []
  index
    .search({
      pageSize: batchSize
    })
    .on('data', function(doc) {
      ids.push(doc.id)
    }).on('end', function() {
      t.is(ids.length, batchSize)
    })
})

test('delete docs', function (t) {
  t.plan(batchSize)
  ids.forEach(function(id) {
    index.del([id], function(err) {
      t.error(err)
    })
  })
})

// TODO: make the following test work

// test('delete docs', function (t) {
//   t.plan(1)
//   index.concurrentDel(ids, function(err) {
//     t.error(err)
//   })
// })


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
