const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'CREATED'

let timestamp

const opts = {}
if (typeof window === 'undefined') {
  const { ClassicLevel } = require('classic-level')
  opts.db = new ClassicLevel(indexName)
}

test('create index', t => {
  t.plan(1)
  si({ name: indexName, ...opts }).then(db => {
    global[indexName] = db
    t.ok(db, !undefined)
  })
})

test('timestamp was created', t => {
  t.plan(1)
  global[indexName].INDEX.STORE.get(['~CREATED'], global[indexName].INDEX.LEVEL_OPTIONS).then(created => {
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

test('recreate index', t => {
  t.plan(1)
  si({ name: indexName, ...opts }).then(db => {
    global[indexName] = db
    t.ok(db, !undefined)
  })
})

test('CREATED timestamp is unchanged after db is closed and reopened', t => {
  t.plan(1)
  global[indexName].CREATED().then(res => t.equals(res, timestamp))
})
