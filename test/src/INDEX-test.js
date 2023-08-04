import test from 'tape'

const { SearchIndex } = await import(
  '../../src/' + process.env.SI_TEST_ENTRYPOINT
)

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'INDEX'
const global = {}

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({ name: indexName })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
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
  global[indexName].INDEX.STORE.get(
    ['IDX', 'brand', ['volvo', '1.00']],
    global[indexName].INDEX.LEVEL_OPTIONS
  ).then(result => t.deepEquals(result, [0]))
})
