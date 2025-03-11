import test from 'tape'

import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'CREATED'
const global = {}

let timestamp

test('create a search index', t => {
  t.plan(1)
  try {
    global[indexName] = new SearchIndex({
      name: indexName
    })
    // Timeout here to deal with there no longer being a callback/promise on the
    // constructor
    setTimeout(() => {
      t.ok(global[indexName])
    }, 10)
  } catch (e) {
    t.error(e)
  }
})

test('timeout here to deal with there no longer being a callback/promise on the constructor', t => {
  t.plan(1)
  setTimeout(() => {
    t.ok(global[indexName])
  }, 100)
})

test('timestamp was created', t => {
  t.plan(1)
  global[indexName].INDEX.STORE.get(['~CREATED']).then(created => {
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
  global[indexName].INDEX.STORE.close().then(() => {
    global[indexName] = null
    t.ok('closed')
  })
})

test('confirm index is closed', t => {
  t.plan(1)
  t.equals(global[indexName], null)
})

test('reopen index', t => {
  t.plan(1)
  try {
    global[indexName] = new SearchIndex({
      name: indexName
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
