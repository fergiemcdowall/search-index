import test from 'tape'

import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'CACHE'
const global = {}

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({
      name: indexName,
      cacheLength: 5 // TODO: this needs to be updated
    })
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
      brand: 'Volvo',
      colour: 'yellow'
    },
    {
      _id: 1,
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      colour: 'red'
    },
    {
      _id: 2,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colour: 'blue'
    },
    {
      _id: 3,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW',
      colour: 'red'
    },
    {
      _id: 4,
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      colour: 'red'
    },
    {
      _id: 5,
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colour: 'blue'
    },
    {
      _id: 6,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW',
      colour: 'yellow'
    },
    {
      _id: 7,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla',
      colour: 'yellow'
    },
    {
      _id: 8,
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla',
      colour: 'blue'
    },
    {
      _id: 9,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colour: 'red'
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('can inspect cache', t => {
  t.plan(1)
  t.deepEquals(Array.from(global[indexName]._CACHE.keys()), [])
})

test('query', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        GET: 'brand:tesla'
      },
      {
        BUCKETS: [
          {
            FIELD: 'make',
            VALUE: 'volvo'
          }
        ]
      }
    )
    .then(res => {
      t.deepEqual(res, {
        BUCKETS: [
          { FIELD: ['make'], VALUE: { GTE: 'volvo', LTE: 'volvo' }, _id: [8] }
        ],
        RESULT: [
          {
            _id: 7,
            _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }]
          },
          {
            _id: 8,
            _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }]
          }
        ],
        RESULT_LENGTH: 2
      })
    })
})

test('inspect cache', t => {
  t.plan(2)
  t.deepEquals(
    global[indexName]._CACHE.keys().next().value,
    '{"QUERY":[{"GET":"brand:tesla"},{"BUCKETS":[{"FIELD":"make","VALUE":"volvo"}]}]}'
  )
  t.deepEquals(global[indexName]._CACHE.values().next().value, {
    RESULT: [
      {
        _id: 7,
        _match: [
          {
            FIELD: 'brand',
            VALUE: 'tesla',
            SCORE: '1.00'
          }
        ]
      },
      {
        _id: 8,
        _match: [
          {
            FIELD: 'brand',
            VALUE: 'tesla',
            SCORE: '1.00'
          }
        ]
      }
    ],
    RESULT_LENGTH: 2,
    BUCKETS: [
      {
        FIELD: ['make'],
        VALUE: {
          GTE: 'volvo',
          LTE: 'volvo'
        },
        _id: [8]
      }
    ]
  })
})

test('run a duplicate query', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        GET: 'brand:tesla'
      },
      {
        BUCKETS: [
          {
            FIELD: 'make',
            VALUE: 'volvo'
          }
        ]
      }
    )
    .then(res => {
      t.deepEqual(res, {
        BUCKETS: [
          { FIELD: ['make'], VALUE: { GTE: 'volvo', LTE: 'volvo' }, _id: [8] }
        ],
        RESULT: [
          {
            _id: 7,
            _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }]
          },
          {
            _id: 8,
            _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }]
          }
        ],
        RESULT_LENGTH: 2
      })
    })
})

test('cache still only contains 1 entry', t => {
  t.plan(1)
  t.deepEquals(
    global[indexName]._CACHE.keys().next().value,
    '{"QUERY":[{"GET":"brand:tesla"},{"BUCKETS":[{"FIELD":"make","VALUE":"volvo"}]}]}'
  )
})

test('run more queries', t => {
  // const { DOCUMENTS, DICTIONARY, QUERY, SEARCH, _CACHE } = global[indexName]
  t.plan(1)
  global[indexName]._CACHE.clear()
  global[indexName]
    .QUERY('one')
    .then(res => global[indexName].QUERY('two'))
    .then(res => global[indexName].QUERY('two'))
    .then(res => global[indexName].QUERY('two'))
    .then(res => global[indexName].QUERY('three'))
    .then(res => global[indexName].QUERY('three'))
    .then(res => global[indexName].QUERY('three'))
    .then(res => global[indexName].DICTIONARY())
    .then(res => global[indexName].DOCUMENTS())
    .then(res => global[indexName].QUERY('three'))
    .then(res => global[indexName].QUERY('three'))
    .then(res => global[indexName].SEARCH('tesla'))
    .then(res => global[indexName].SEARCH('tesla'))
    .then(res => global[indexName].SEARCH(['tesla']))
    .then(res => global[indexName].QUERY('three'))
    .then(res => global[indexName].QUERY('four'))
    .then(res => t.pass('done'))
})

test('cache has stripped all duplicate entries', t => {
  const keys = [
    '{"QUERY":["four",null]}',
    '{"QUERY":["three",null]}',
    '{"SEARCH":[["tesla"],null]}',
    '{"SEARCH":["tesla",null]}',
    '{"DOCUMENTS":[]}',
    '{"DICTIONARY":null}',
    '{"QUERY":["two",null]}',
    '{"QUERY":["one",null]}'
  ]
  t.plan(keys.length)
  const cKeys = global[indexName]._CACHE.keys()
  keys.forEach(item => t.deepEquals(cKeys.next().value, item))
})

test('bump oldest cache entry to newest', t => {
  t.plan(1)
  global[indexName].QUERY('two').then(res => t.pass('done'))
})

test('oldest cache entry is now newest', t => {
  const keys = [
    '{"QUERY":["two",null]}',
    '{"QUERY":["four",null]}',
    '{"QUERY":["three",null]}',
    '{"SEARCH":[["tesla"],null]}',
    '{"SEARCH":["tesla",null]}',
    '{"DOCUMENTS":[]}',
    '{"DICTIONARY":null}',
    '{"QUERY":["one",null]}'
  ]
  t.plan(keys.length)
  const cKeys = global[indexName]._CACHE.keys()
  keys.forEach(item => t.deepEquals(cKeys.next().value, item))
})

test('adding a new document clears the cache', t => {
  t.plan(1)
  global[indexName]
    .PUT([
      {
        _id: 10,
        make: 'Tesla',
        manufacturer: 'Volvo',
        brand: 'Volvo',
        colour: 'Gold'
      }
    ])
    .then(t.pass)
})

test('cache is now cleared', t => {
  const { _CACHE } = global[indexName]
  t.plan(1)
  t.deepEquals(_CACHE.keys().next(), { value: undefined, done: true })
})

test('bump oldest cache entry to newest', t => {
  t.plan(1)
  global[indexName].QUERY('boooom').then(res => t.pass('done'))
})

test('cache is filling up again', t => {
  t.plan(1)
  t.deepEquals(
    global[indexName]._CACHE.keys().next().value,
    '{"QUERY":["boooom",null]}'
  )
})
