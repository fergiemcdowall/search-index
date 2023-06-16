const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'CACHE'

test('create a search index', t => {
  t.plan(1)
  si({
    name: indexName,
    cacheLength: 5 // TODO: this needs to be updated
  }).then(db => {
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
  const { _CACHE } = global[indexName]
  t.plan(1)
  // t.deepEquals(Array.from(_CACHE.LRUStore.keys()), [])
  t.deepEquals(Array.from(_CACHE.keys()), [])
})

test('query', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY(
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
  ).then(res => {
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
  const { _CACHE } = global[indexName]
  t.plan(2)
  t.deepEquals(
    _CACHE.keys().next().value,
    '{"QUERY":[{"GET":"brand:tesla"},{"BUCKETS":[{"FIELD":"make","VALUE":"volvo"}]}]}'
  )
  t.deepEquals(_CACHE.values().next().value, {
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
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY(
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
  ).then(res => {
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
  const { _CACHE } = global[indexName]
  t.plan(1)
  t.deepEquals(
    _CACHE.keys().next().value,
    '{"QUERY":[{"GET":"brand:tesla"},{"BUCKETS":[{"FIELD":"make","VALUE":"volvo"}]}]}'
  )
})

test('run more queries', t => {
  const { DOCUMENTS, DICTIONARY, QUERY, SEARCH, _CACHE } = global[indexName]
  t.plan(1)
  _CACHE.clear()
  QUERY('one')
    .then(res => QUERY('two'))
    .then(res => QUERY('two'))
    .then(res => QUERY('two'))
    .then(res => QUERY('three'))
    .then(res => QUERY('three'))
    .then(res => QUERY('three'))
    .then(res => DICTIONARY())
    .then(res => DOCUMENTS())
    .then(res => QUERY('three'))
    .then(res => QUERY('three'))
    .then(res => SEARCH('tesla'))
    .then(res => SEARCH('tesla'))
    .then(res => SEARCH(['tesla']))
    .then(res => QUERY('three'))
    .then(res => QUERY('four'))
    .then(res => t.pass('done'))
})

test('cache has stripped all duplicate entries', t => {
  const { _CACHE } = global[indexName]
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
  const cKeys = _CACHE.keys()
  keys.forEach(item => t.deepEquals(cKeys.next().value, item))
})

test('bump oldest cache entry to newest', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY('two').then(res => t.pass('done'))
})

test('oldest cache entry is now newest', t => {
  const { _CACHE } = global[indexName]
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
  const cKeys = _CACHE.keys()
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
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY('boooom').then(res => t.pass('done'))
})

test('cache is filling up again', t => {
  const { _CACHE } = global[indexName]
  t.plan(1)
  t.deepEquals(_CACHE.keys().next().value, '{"QUERY":["boooom",null]}')
})
