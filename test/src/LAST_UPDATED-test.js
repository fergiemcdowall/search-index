import test from 'tape'

const { SearchIndex } = await import(
  '../../src/' + process.env.SI_TEST_ENTRYPOINT
)

let timestamp

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'LAST_UPDATED'
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

test('timestamp was created', t => {
  t.plan(1)
  global[indexName].INDEX.STORE.get(
    ['~LAST_UPDATED'],
    global[indexName].INDEX.LEVEL_OPTIONS
  ).then(lastUpdated => {
    timestamp = lastUpdated
    return t.pass('timestamp created')
  })
})

test('can read LAST_UPDATED timestamp with API', t => {
  t.plan(1)
  global[indexName].LAST_UPDATED().then(res => t.equals(res, timestamp))
})

test('is valid timestamp', t => {
  t.plan(1)
  global[indexName].INDEX.STORE.get(
    ['~LAST_UPDATED'],
    global[indexName].INDEX.LEVEL_OPTIONS
  ).then(lastUpdated => t.ok(new Date(lastUpdated)))
})

test('update index', t => {
  t.plan(1)
  setTimeout(function () {
    // wait to ensure that newer timestamp is bigger
    global[indexName]
      .PUT([
        {
          _id: 0,
          body: 'test doc'
        }
      ])
      .then(res => t.ok(true))
  }, 100)
})

test('LAST_UPDATED timestamp has increased', t => {
  t.plan(1)
  global[indexName].LAST_UPDATED().then(res => t.ok(res > timestamp))
})
