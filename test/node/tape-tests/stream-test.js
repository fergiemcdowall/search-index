const test = require('tape')
const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const batchSize = 100

var index

test('init search index', function (t) {
  t.plan(1)
  SearchIndex({
    indexPath: 'test/sandbox/stream-test-1'
  }, function(err, idx) {
    t.error(err)
    index = idx
  })
})

test('add docs using a stream', function (t) {
  t.plan(1)
  var i = 0
  var s = new Readable({ objectMode: true })
  for (var i = 0; i < batchSize; i++) {
    var text = 'this is the amazing do number ' + i
    if (i%2 == 0) text += ' amazing'
    s.push({
      id: i + 'a',
      text: text
    })
  }
  s.push(null)
  s.pipe(index.feed({ objectMode: true }))
    .on('finish', function () {
      t.pass('ended ok')
    })
})

test('verify that large values are sorted', function (t) {
  t.plan(3)
  index.options.indexes.get('TF￮text￮number', function (err, val) {
    t.error(err)
    var valClone = JSON.parse(JSON.stringify(val))
    valClone.sort(function(a, b) {
      if (a[0] < b[0]) return 1
      if (a[0] > b[0]) return -1      
      if (a[1] < b[1]) return 1
      if (a[1] > b[1]) return -1
      return 0
    })
    t.deepEqual(val, valClone)
    t.equal(val.length, batchSize)
  })
})

test('verify that all docs created a DOCUMENT entry', function (t) {
  t.plan(batchSize)
  var i = 0
  index.options.indexes.createReadStream({
    gte: 'DOCUMENT￮',
    lte: 'DOCUMENT￮￮'
  }).on('data', function (d) {
    t.pass('got doc number ' + ++i)
  }).on('error', function (e) {
    t.error(e)
  })
})
