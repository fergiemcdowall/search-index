import { EntryStream } from 'level-read-stream'
import test from 'tape'
import { SearchIndex } from '../../src/main.js'
import { packageVersion } from '../../src/version.js'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'storeVector-test'
const global = {}

const data = [
  {
    _id: 1,
    text: 'a giant banana'
  },
  {
    _id: 2,
    text: 'a giant pineapple'
  },
  {
    _id: 3,
    text: 'a small pineapple'
  }
]

test('create a search index with storeVectors: true', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({
      name: indexName,
      storeVectors: true
    })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add some data', t => {
  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('can verify store', t => {
  const entries = [
    {
      key: ['CREATED_WITH'],
      value: 'search-index@' + packageVersion
    },
    {
      key: ['DOC', 1],
      value: {
        _id: 1,
        text: [
          ['a', '1.00'],
          ['banana', '1.00'],
          ['giant', '1.00']
        ]
      }
    },
    {
      key: ['DOC', 2],
      value: {
        _id: 2,
        text: [
          ['a', '1.00'],
          ['giant', '1.00'],
          ['pineapple', '1.00']
        ]
      }
    },
    {
      key: ['DOC', 3],
      value: {
        _id: 3,
        text: [
          ['a', '1.00'],
          ['pineapple', '1.00'],
          ['small', '1.00']
        ]
      }
    },
    { key: ['DOCUMENT_COUNT'], value: 3 },
    { key: ['DOC_RAW', 1], value: { _id: 1, text: 'a giant banana' } },
    { key: ['DOC_RAW', 2], value: { _id: 2, text: 'a giant pineapple' } },
    { key: ['DOC_RAW', 3], value: { _id: 3, text: 'a small pineapple' } },
    { key: ['FIELD', 'text'], value: 'text' },
    { key: ['IDX', 'text', ['a', '1.00']], value: [1, 2, 3] },
    { key: ['IDX', 'text', ['banana', '1.00']], value: [1] },
    { key: ['IDX', 'text', ['giant', '1.00']], value: [1, 2] },
    {
      key: ['IDX', 'text', ['pineapple', '1.00']],
      value: [2, 3]
    },
    { key: ['IDX', 'text', ['small', '1.00']], value: [3] }
  ]
  t.plan(entries.length + 1)
  new EntryStream(global[indexName].INDEX.STORE, {
    lt: ['~'],
    ...global[indexName].INDEX.LEVEL_OPTIONS
  })
    .on('data', d => {
      t.deepEquals(d, entries.shift())
    })
    .on('end', resolve => t.pass('ended'))
})

test('create another search index with storeVectors: false', async t => {
  t.plan(1)
  try {
    global[indexName + '1'] = await new SearchIndex({
      name: indexName + '1',
      storeVectors: false
    })
    t.ok(global[indexName + '1'])
  } catch (e) {
    t.error(e)
  }
})

test('create another search index with storeVectors: false', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({
      name: indexName,
      storeVectors: false
    })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add some data', t => {
  t.plan(1)
  global[indexName + '1'].PUT(data).then(t.pass)
})

test('can verify store', t => {
  const entries = [
    {
      key: ['CREATED_WITH'],
      value: 'search-index@' + packageVersion
    },
    { key: ['DOCUMENT_COUNT'], value: 3 },
    { key: ['DOC_RAW', 1], value: { _id: 1, text: 'a giant banana' } },
    { key: ['DOC_RAW', 2], value: { _id: 2, text: 'a giant pineapple' } },
    { key: ['DOC_RAW', 3], value: { _id: 3, text: 'a small pineapple' } },
    { key: ['FIELD', 'text'], value: 'text' },
    { key: ['IDX', 'text', ['a', '1.00']], value: [1, 2, 3] },
    { key: ['IDX', 'text', ['banana', '1.00']], value: [1] },
    { key: ['IDX', 'text', ['giant', '1.00']], value: [1, 2] },
    {
      key: ['IDX', 'text', ['pineapple', '1.00']],
      value: [2, 3]
    },
    { key: ['IDX', 'text', ['small', '1.00']], value: [3] }
  ]
  t.plan(entries.length + 1)
  new EntryStream(global[indexName + '1'].INDEX.STORE, {
    lt: ['~'],
    ...global[indexName].INDEX.LEVEL_OPTIONS
  })
    .on('data', d => {
      // console.log(d)
      t.deepEquals(d, entries.shift())
    })
    .on('end', resolve => t.pass('ended'))
})

test('create another search index with storeVectors not specified (default true)', async t => {
  t.plan(1)
  try {
    global[indexName + '2'] = await new SearchIndex({
      name: indexName + '2'
    })
    t.ok(global[indexName + '2'])
  } catch (e) {
    t.error(e)
  }
})

test('can add some data', t => {
  t.plan(1)
  global[indexName + '2'].PUT(data).then(t.pass)
})

test('can verify store', t => {
  const entries = [
    {
      key: ['CREATED_WITH'],
      value: 'search-index@' + packageVersion
    },
    {
      key: ['DOC', 1],
      value: {
        _id: 1,
        text: [
          ['a', '1.00'],
          ['banana', '1.00'],
          ['giant', '1.00']
        ]
      }
    },
    {
      key: ['DOC', 2],
      value: {
        _id: 2,
        text: [
          ['a', '1.00'],
          ['giant', '1.00'],
          ['pineapple', '1.00']
        ]
      }
    },
    {
      key: ['DOC', 3],
      value: {
        _id: 3,
        text: [
          ['a', '1.00'],
          ['pineapple', '1.00'],
          ['small', '1.00']
        ]
      }
    },
    { key: ['DOCUMENT_COUNT'], value: 3 },
    { key: ['DOC_RAW', 1], value: { _id: 1, text: 'a giant banana' } },
    { key: ['DOC_RAW', 2], value: { _id: 2, text: 'a giant pineapple' } },
    { key: ['DOC_RAW', 3], value: { _id: 3, text: 'a small pineapple' } },
    { key: ['FIELD', 'text'], value: 'text' },
    { key: ['IDX', 'text', ['a', '1.00']], value: [1, 2, 3] },
    { key: ['IDX', 'text', ['banana', '1.00']], value: [1] },
    { key: ['IDX', 'text', ['giant', '1.00']], value: [1, 2] },
    {
      key: ['IDX', 'text', ['pineapple', '1.00']],
      value: [2, 3]
    },
    { key: ['IDX', 'text', ['small', '1.00']], value: [3] }
  ]
  t.plan(entries.length + 1)
  new EntryStream(global[indexName + '2'].INDEX.STORE, {
    lt: ['~'],
    ...global[indexName].INDEX.LEVEL_OPTIONS
  })
    .on('data', d => {
      // console.log(d)
      t.deepEquals(d, entries.shift())
    })
    .on('end', resolve => t.pass('ended'))
})
