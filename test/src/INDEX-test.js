const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'INDEX'

test('create a search index', t => {
  t.plan(1)
  si({ name: indexName }).then(db => {
    global[indexName] = db
    t.pass('ok')
  })
})

test('can add data', t => {
  const data = [
    {
      _id: 0,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    }
  ]
  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('Can access the underlying fergies-inverted-index', t => {
  t.plan(1)
  global[indexName].INDEX.STORE.get(['IDX', 'brand', ['volvo', '1.00']], global[indexName].INDEX.LEVEL_OPTIONS).then(
    result => t.deepEquals(result, [0])
  )
})
