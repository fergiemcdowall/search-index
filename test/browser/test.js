const SearchIndex = require('../../')
const test = require('tape')
const Readable = require('stream').Readable

var si

test ('general testiness', function (assert) {
  assert.plan(1)
  assert.equals(true, true)
})

test ('can instantiate a search-index', function (assert) {
  assert.plan(1)
  SearchIndex({}, function (err, thisSi) {
    si = thisSi
    assert.error(err)
  })
})

test ('can index data', function (assert) {
  assert.plan(1)
  si.concurrentAdd({}, [
    {id: 'one', body: 'the first doc'},
    {id: 'two', body: 'the second doc'},
    {id: 'three', body: 'the third doc'}
  ], function(err) {
    assert.error(err)
  })
})

test ('can search data', function (assert) {
  assert.plan(3)
  var expectedResults = [
    {id: 'two', body: 'the second doc'},
    {id: 'three', body: 'the third doc'},
    {id: 'one', body: 'the first doc'}
  ]
  si.search().on('data', function (d) {
    assert.looseEquals(d.document, expectedResults.shift())
  })
})

test ('close signal', function (assert) {
  window.close()
})

