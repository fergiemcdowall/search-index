import { SearchIndex } from '../../src/main.js'
import test from 'tape'
import Level from 'level'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'CREATED'
const global = {}

let timestamp

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({
      name: indexName,
      db: new Level(indexName, { valueEncoding: 'json' })
    })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('timestamp was created', t => {
  t.plan(1)
  global[indexName].INDEX.STORE.get(
    ['~CREATED'],
    global[indexName].INDEX.LEVEL_OPTIONS
  ).then(created => {
    timestamp = created
    return t.pass('timestamp created')
  })
})

test('can read CREATED timestamp with API', t => {
  t.plan(1)
  global[indexName].CREATED().then(res => t.equals(res, timestamp))
})

test('closing instance', t => {
  t.plan(1)
  global[indexName].INDEX.STORE.close(() => {
    global[indexName] = null
    t.ok('closed')
  })
})

test('confirm index is closed', t => {
  t.plan(1)
  t.equals(global[indexName], null)
})

test('reopen index', async t => {
  try {
    global[indexName] = await new SearchIndex({
      name: indexName,
      db: new Level(indexName, { valueEncoding: 'json' })
    })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('CREATED timestamp is unchanged after db is closed and reopened', t => {
  t.plan(1)
  global[indexName].CREATED().then(res => t.equals(res, timestamp))
})
