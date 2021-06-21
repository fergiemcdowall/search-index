const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'QUERY'

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
    },
    {
      _id: 1,
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: 2,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    },
    {
      _id: 3,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW'
    },
    {
      _id: 4,
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: 5,
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    },
    {
      _id: 6,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW'
    },
    {
      _id: 7,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla'
    },
    {
      _id: 8,
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla'
    },
    {
      _id: 9,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

// AND
test('simple AND with 2 clauses', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    AND: ['make:volvo', 'manufacturer:bmw']
  }).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 8,
          _match: [
            { FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'bmw', SCORE: '1.00' }
          ]
        }
      ],
      RESULT_LENGTH: 1
    })
  })
})

// BUCKET
test('simple BUCKET', t => {
  const { BUCKETS } = global[indexName]
  t.plan(1)
  BUCKETS({
    FIELD: 'make',
    VALUE: 'volvo'
  }).then(res => {
    t.deepEqual(res, [
      {
        FIELD: ['make'],
        VALUE: { GTE: 'volvo', LTE: 'volvo' },
        _id: [4, 5, 8]
      }
    ])
  })
})

// AGGREGATE
test('simple AGGREGATE', t => {
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

// QUERY with FACETS
test('QUERY with FACETS', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY(
    {
      GET: {
        FIELD: 'brand',
        VALUE: 'tesla'
      }
    },
    {
      FACETS: [
        {
          FIELD: 'make'
        }
      ]
    }
  ).then(res => {
    t.deepEqual(res, {
      RESULT: [
        { _id: 7, _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }] },
        { _id: 8, _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }] }
      ],
      RESULT_LENGTH: 2,
      FACETS: [
        { FIELD: 'make', VALUE: 'bmw', _id: [7] },
        { FIELD: 'make', VALUE: 'tesla', _id: [] },
        { FIELD: 'make', VALUE: 'volvo', _id: [8] }
      ]
    })
  })
})

test('QUERY with FACETS where query gives an empty result', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY(
    {
      GET: {
        FIELD: 'brand',
        VALUE: 'teslaXXXXX'
      }
    },
    {
      FACETS: [
        {
          FIELD: 'make'
        }
      ]
    }
  ).then(res => {
    t.deepEqual(res, {
      RESULT: [],
      RESULT_LENGTH: 0,
      FACETS: []
    })
  })
})

// QUERY
test('simple QUERY', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    GET: 'make:volvo'
  }).then(res => {
    t.deepEqual(res, {
      RESULT: [
        { _id: 4, _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] },
        { _id: 5, _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] },
        { _id: 8, _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] }
      ],
      RESULT_LENGTH: 3
    })
  })
})

// NOT
test('simple NOT', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    NOT: {
      INCLUDE: 'manufacturer:tesla',
      EXCLUDE: 'brand:volvo'
    }
  }).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 6,
          _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: 7,
          _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }]
        }
      ],
      RESULT_LENGTH: 2
    })
  })
})

test('simple NOT with DOCUMENTS', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY(
    {
      NOT: {
        INCLUDE: 'manufacturer:tesla',
        EXCLUDE: 'brand:volvo'
      }
    },
    { DOCUMENTS: true }
  ).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 6,
          _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
          _doc: {
            _id: 6,
            make: 'Tesla',
            manufacturer: 'Tesla',
            brand: 'BMW'
          }
        },
        {
          _id: 7,
          _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
          _doc: {
            _id: 7,
            make: 'BMW',
            manufacturer: 'Tesla',
            brand: 'Tesla'
          }
        }
      ],
      RESULT_LENGTH: 2
    })
  })
})

// OR
test('simple OR with 2 clauses', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    OR: ['make:volvo', 'brand:tesla']
  }).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 4,
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: 5,
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: 8,
          _match: [
            { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }
          ]
        },
        {
          _id: 7,
          _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }]
        }
      ],
      RESULT_LENGTH: 4
    })
  })
})

// SEARCH
test('simple SEARCH', t => {
  const { SEARCH } = global[indexName]
  t.plan(1)
  SEARCH(['tesla']).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 2,
          _match: [
            { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
          ],
          _score: 0.64
        },
        {
          _id: 6,
          _match: [
            { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
          ],
          _score: 0.64
        },
        {
          _id: 7,
          _match: [
            { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
          ],
          _score: 0.64
        },
        {
          _id: 0,
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 0.32
        },
        {
          _id: 3,
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 0.32
        },
        {
          _id: 5,
          _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 0.32
        },
        {
          _id: 8,
          _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 0.32
        },
        {
          _id: 9,
          _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 0.32
        }
      ],
      RESULT_LENGTH: 8
    })
  })
})
