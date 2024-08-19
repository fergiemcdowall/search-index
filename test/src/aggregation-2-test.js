import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_AGGREGATE'
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
      _id: '0',
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: '1',
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: '2',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    },
    {
      _id: '3',
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW'
    },
    {
      _id: '4',
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: '5',
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    },
    {
      _id: '6',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW'
    },
    {
      _id: '7',
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla'
    },
    {
      _id: '8',
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla'
    },
    {
      _id: '9',
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('simple aggregation', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        AND: ['make:bmw']
      },
      {
        BUCKETS: [
          {
            FIELD: 'make',
            VALUE: {
              GTE: 'a',
              LTE: 'u'
            }
          }
        ]
      }
    )
    .then(res => {
      t.deepEqual(res, {
        BUCKETS: [
          {
            FIELD: ['make'],
            VALUE: { GTE: 'a', LTE: 'u' },
            _id: ['1', '7', '9']
          }
        ],
        RESULT_LENGTH: 3,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 },
        RESULT: [
          {
            _id: '1',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
          },
          {
            _id: '7',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
          },
          { _id: '9', _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] }
        ]
      })
    })
})

test('simple AGGREGATE (JSON)', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        AND: ['make:bmw', 'manufacturer:tesla']
      },
      {
        BUCKETS: [
          {
            FIELD: 'make',
            VALUE: {
              GTE: 'a',
              LTE: 'u'
            }
          }
        ]
      }
    )
    .then(res => {
      t.deepEqual(res, {
        BUCKETS: [
          {
            FIELD: ['make'],
            VALUE: { GTE: 'a', LTE: 'u' },
            _id: ['7', '9']
          }
        ],
        RESULT: [
          {
            _id: '7',
            _match: [
              { FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ]
          },
          {
            _id: '9',
            _match: [
              { FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ]
          }
        ],
        RESULT_LENGTH: 2,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
      })
    })
})

test('simple AGGREGATE (JSON)', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        OR: ['brand:tesla', 'manufacturer:tesla']
      },
      {
        BUCKETS: [
          {
            FIELD: 'make',
            VALUE: {
              GTE: 'a',
              LTE: 'u'
            }
          }
        ]
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '7',
            _match: [
              { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ]
          },
          {
            _id: '8',
            _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }]
          },
          {
            _id: '2',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }]
          },
          {
            _id: '5',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }]
          },
          {
            _id: '6',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }]
          },
          {
            _id: '9',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }]
          }
        ],
        RESULT_LENGTH: 6,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 },
        BUCKETS: [
          {
            FIELD: ['make'],
            VALUE: { GTE: 'a', LTE: 'u' },
            _id: ['2', '6', '7', '9']
          }
        ]
      })
    })
})

test('simple aggregation', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        GET: 'make:bmw'
      },
      {
        FACETS: [
          {
            FIELD: 'make',
            VALUE: {
              GTE: 'a',
              LTE: 'u'
            }
          }
        ]
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '1',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
          },
          {
            _id: '7',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
          },
          { _id: '9', _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] }
        ],
        RESULT_LENGTH: 3,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 },
        FACETS: [{ FIELD: 'make', VALUE: 'bmw', _id: ['1', '7', '9'] }]
      })
    })
})

test('simple AGGREGATE, using DISTINCT (JSON)', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        GET: {
          FIELD: ['make'],
          VALUE: {
            GTE: 'a',
            LTE: 'c'
          }
        }
      },
      {
        FACETS: [
          {
            FIELD: 'make',
            VALUE: {
              GTE: 'a',
              LTE: 'u'
            }
          }
        ]
      }
    )
    .then(res => {
      t.deepEqual(res, {
        FACETS: [{ FIELD: 'make', VALUE: 'bmw', _id: ['1', '7', '9'] }],
        RESULT: [
          {
            _id: '1',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
          },
          {
            _id: '7',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
          },
          { _id: '9', _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] }
        ],
        RESULT_LENGTH: 3,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
      })
    })
})

test('simple aggregation', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        GET: {
          FIELD: 'make',
          VALUE: 'bmw'
        }
      },
      {
        FACETS: [
          {
            FIELD: 'make',
            VALUE: {
              GTE: 'a',
              LTE: 'u'
            }
          }
        ]
      }
    )
    .then(res => {
      t.deepEqual(res, {
        FACETS: [{ FIELD: 'make', VALUE: 'bmw', _id: ['1', '7', '9'] }],
        RESULT: [
          {
            _id: '1',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
          },
          {
            _id: '7',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
          },
          { _id: '9', _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] }
        ],
        RESULT_LENGTH: 3,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
      })
    })
})

test('simple aggregation, return full documents', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        GET: {
          FIELD: 'make',
          VALUE: 'bmw'
        }
      },
      {
        DOCUMENTS: true,
        FACETS: [
          {
            FIELD: 'make',
            VALUE: {
              GTE: 'a',
              LTE: 'u'
            }
          }
        ]
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '1',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }],
            _doc: {
              _id: '1',
              make: 'BMW',
              manufacturer: 'Volvo',
              brand: 'Volvo'
            }
          },
          {
            _id: '7',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }],
            _doc: {
              _id: '7',
              make: 'BMW',
              manufacturer: 'Tesla',
              brand: 'Tesla'
            }
          },
          {
            _id: '9',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }],
            _doc: {
              _id: '9',
              make: 'BMW',
              manufacturer: 'Tesla',
              brand: 'Volvo'
            }
          }
        ],
        RESULT_LENGTH: 3,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 },
        FACETS: [{ FIELD: 'make', VALUE: 'bmw', _id: ['1', '7', '9'] }]
      })
    })
})
