const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'LAST_UPDATED'

let timestamp

test('create index', t => {
  t.plan(1)
  si({ name: indexName }).then(db => {
    global[indexName] = db
    t.ok(db, !undefined)
  })
})

test('timestamp was created', t => {
  t.plan(1)
  global[indexName].INDEX.STORE.get(['~LAST_UPDATED'], global[indexName].INDEX.LEVEL_OPTIONS).then(lastUpdated => {
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
  global[indexName].INDEX.STORE.get(['~LAST_UPDATED'], global[indexName].INDEX.LEVEL_OPTIONS).then(lastUpdated =>
    t.ok(new Date(lastUpdated))
  )
})

test('update index', t => {
  t.plan(1)
  global[indexName]
    .PUT([
      {
        _id: 0,
        body: 'test doc'
      }
    ])
    .then(res => t.ok(true))
})

test('LAST_UPDATED timestamp has increased', t => {
  t.plan(1)
  global[indexName].LAST_UPDATED().then(res => t.ok(res > timestamp))
})
