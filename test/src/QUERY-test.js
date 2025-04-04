import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'QUERY'
const global = {}

test('create a search index', t => {
  t.plan(1)
  try {
    global[indexName] = new SearchIndex({ name: indexName })
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
  t.plan(1)
  global[indexName]
    .QUERY({
      AND: ['make:volvo', 'manufacturer:bmw']
    })
    .then(res => {
      t.deepEqual(res, {
        QUERY: { AND: ['make:volvo', 'manufacturer:bmw'] },
        OPTIONS: {},
        RESULT: [
          {
            _id: 8,
            _match: [
              { FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'bmw', SCORE: '1.00' }
            ]
          }
        ],
        RESULT_LENGTH: 1,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
      })
    })
})

// BUCKET
test('simple BUCKET', t => {
  t.plan(1)
  global[indexName]
    .BUCKETS({
      FIELD: 'make',
      VALUE: 'volvo'
    })
    .then(res => {
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
        QUERY: { GET: 'brand:tesla' },
        OPTIONS: {
          BUCKETS: [{ FIELD: ['make'], VALUE: { GTE: 'volvo', LTE: 'volvo' } }]
        },
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
        RESULT_LENGTH: 2,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
      })
    })
})

// QUERY with FACETS
test('QUERY with FACETS', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
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
    )
    .then(res => {
      t.deepEqual(res, {
        QUERY: {
          GET: { FIELD: ['brand'], VALUE: { GTE: 'tesla', LTE: 'tesla' } }
        },
        OPTIONS: {
          FACETS: [{ FIELD: ['make'], VALUE: { GTE: null, LTE: undefined } }]
        },
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
        RESULT_LENGTH: 2,
        FACETS: [
          { FIELD: 'make', VALUE: 'bmw', _id: [7] },
          { FIELD: 'make', VALUE: 'volvo', _id: [8] }
        ],
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
      })
    })
})

test('QUERY with FACETS where query gives an empty result', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
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
    )
    .then(res => {
      t.deepEqual(res, {
        QUERY: {
          GET: {
            FIELD: ['brand'],
            VALUE: { GTE: 'teslaXXXXX', LTE: 'teslaXXXXX' }
          }
        },
        OPTIONS: { FACETS: [{ FIELD: 'make' }] },
        RESULT: [],
        RESULT_LENGTH: 0,
        FACETS: [],
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 0, DOC_OFFSET: 0 }
      })
    })
})

// QUERY
test('simple QUERY', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      GET: 'make:volvo'
    })
    .then(res => {
      t.deepEqual(res, {
        QUERY: { GET: 'make:volvo' },
        OPTIONS: {},
        RESULT: [
          {
            _id: 4,
            _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
          },
          {
            _id: 5,
            _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
          },
          { _id: 8, _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] }
        ],
        RESULT_LENGTH: 3,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
      })
    })
})

// NOT
test('simple NOT', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      NOT: {
        INCLUDE: 'manufacturer:tesla',
        EXCLUDE: 'brand:volvo'
      }
    })
    .then(res => {
      t.deepEqual(res, {
        QUERY: {
          NOT: { INCLUDE: 'manufacturer:tesla', EXCLUDE: 'brand:volvo' }
        },
        OPTIONS: {},
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
        RESULT_LENGTH: 2,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
      })
    })
})

test('simple NOT with DOCUMENTS', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        NOT: {
          INCLUDE: 'manufacturer:tesla',
          EXCLUDE: 'brand:volvo'
        }
      },
      { DOCUMENTS: true }
    )
    .then(res => {
      t.deepEqual(res, {
        QUERY: {
          NOT: { INCLUDE: 'manufacturer:tesla', EXCLUDE: 'brand:volvo' }
        },
        OPTIONS: { DOCUMENTS: true },
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
        RESULT_LENGTH: 2,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
      })
    })
})

// OR
test('simple OR with 2 clauses', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      OR: ['make:volvo', 'brand:tesla']
    })
    .then(res => {
      t.deepEqual(res, {
        QUERY: { OR: ['make:volvo', 'brand:tesla'] },
        OPTIONS: {},
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
        RESULT_LENGTH: 4,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
      })
    })
})

// SEARCH
test('simple SEARCH', t => {
  t.plan(1)
  global[indexName].SEARCH(['tesla']).then(res => {
    t.deepEqual(res, {
      QUERY: { AND: ['tesla'] },
      OPTIONS: { SCORE: { TYPE: 'TFIDF' }, SORT: true },
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
      RESULT_LENGTH: 8,
      PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
    })
  })
})

// get ALL_DOCUMENTS
test('get all documents', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        ALL_DOCUMENTS: -1
      },
      {
        FACETS: [
          {
            FIELD: 'make'
          }
        ]
      }
    )
    .then(res => {
      t.deepEqual(res, {
        QUERY: { ALL_DOCUMENTS: -1 },
        OPTIONS: {
          FACETS: [{ FIELD: ['make'], VALUE: { GTE: null, LTE: undefined } }]
        },
        RESULT: [
          {
            _id: 0,
            _doc: {
              _id: 0,
              make: 'Tesla',
              manufacturer: 'Volvo',
              brand: 'Volvo'
            }
          },
          {
            _id: 1,
            _doc: {
              _id: 1,
              make: 'BMW',
              manufacturer: 'Volvo',
              brand: 'Volvo'
            }
          },
          {
            _id: 2,
            _doc: {
              _id: 2,
              make: 'Tesla',
              manufacturer: 'Tesla',
              brand: 'Volvo'
            }
          },
          {
            _id: 3,
            _doc: {
              _id: 3,
              make: 'Tesla',
              manufacturer: 'Volvo',
              brand: 'BMW'
            }
          },
          {
            _id: 4,
            _doc: {
              _id: 4,
              make: 'Volvo',
              manufacturer: 'Volvo',
              brand: 'Volvo'
            }
          },
          {
            _id: 5,
            _doc: {
              _id: 5,
              make: 'Volvo',
              manufacturer: 'Tesla',
              brand: 'Volvo'
            }
          },
          {
            _id: 6,
            _doc: {
              _id: 6,
              make: 'Tesla',
              manufacturer: 'Tesla',
              brand: 'BMW'
            }
          },
          {
            _id: 7,
            _doc: {
              _id: 7,
              make: 'BMW',
              manufacturer: 'Tesla',
              brand: 'Tesla'
            }
          },
          {
            _id: 8,
            _doc: {
              _id: 8,
              make: 'Volvo',
              manufacturer: 'BMW',
              brand: 'Tesla'
            }
          },
          {
            _id: 9,
            _doc: {
              _id: 9,
              make: 'BMW',
              manufacturer: 'Tesla',
              brand: 'Volvo'
            }
          }
        ],
        RESULT_LENGTH: 10,
        FACETS: [
          {
            FIELD: 'make',
            VALUE: 'bmw',
            _id: [1, 7, 9]
          },
          {
            FIELD: 'make',
            VALUE: 'tesla',
            _id: [0, 2, 3, 6]
          },
          {
            FIELD: 'make',
            VALUE: 'volvo',
            _id: [4, 5, 8]
          }
        ],
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
      })
    })
})

// get ALL_DOCUMENTS
test('get all documents', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        ALL_DOCUMENTS: -1
      },
      {
        FACETS: [
          {
            FIELD: 'make'
          }
        ],
        PAGE: {
          NUMBER: 0,
          SIZE: 3
        }
      }
    )
    .then(res => {
      t.deepEqual(res, {
        QUERY: { ALL_DOCUMENTS: -1 },
        OPTIONS: {
          FACETS: [{ FIELD: ['make'], VALUE: { GTE: null, LTE: undefined } }],
          PAGE: { NUMBER: 0, SIZE: 3 }
        },
        RESULT: [
          {
            _id: 0,
            _doc: {
              _id: 0,
              make: 'Tesla',
              manufacturer: 'Volvo',
              brand: 'Volvo'
            }
          },
          {
            _id: 1,
            _doc: {
              _id: 1,
              make: 'BMW',
              manufacturer: 'Volvo',
              brand: 'Volvo'
            }
          },
          {
            _id: 2,
            _doc: {
              _id: 2,
              make: 'Tesla',
              manufacturer: 'Tesla',
              brand: 'Volvo'
            }
          }
        ],
        RESULT_LENGTH: 10,
        FACETS: [
          {
            FIELD: 'make',
            VALUE: 'bmw',
            _id: [1, 7, 9]
          },
          {
            FIELD: 'make',
            VALUE: 'tesla',
            _id: [0, 2, 3, 6]
          },
          {
            FIELD: 'make',
            VALUE: 'volvo',
            _id: [4, 5, 8]
          }
        ],
        PAGING: { NUMBER: 0, SIZE: 3, TOTAL: 4, DOC_OFFSET: 0 }
      })
    })
})
