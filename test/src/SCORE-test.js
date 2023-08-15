import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_SCORE'
const global = {}

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({ name: indexName })
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
      brand: 'Volvo',
      price: 3000
    },
    {
      _id: '1',
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      price: 12000
    },
    {
      _id: '2',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      price: 14000
    },
    {
      _id: '3',
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW',
      price: 140000
    },
    {
      _id: '4',
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      price: 1000
    },
    {
      _id: '5',
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      price: 2000
    },
    {
      _id: '6',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW',
      price: 500
    },
    {
      _id: '7',
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla',
      price: 5000
    },
    {
      _id: '8',
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla',
      price: 100
    },
    {
      _id: '9',
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      price: 1000
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('SCORE TFIDF JSON all fields', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        AND: ['tesla']
      },
      {
        SCORE: {
          TYPE: 'TFIDF'
        },
        SORT: true
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '2',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: 0.64
          },
          {
            _id: '6',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: 0.64
          },
          {
            _id: '7',
            _match: [
              { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: 0.64
          },
          {
            _id: '0',
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0.32
          },
          {
            _id: '3',
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0.32
          },
          {
            _id: '5',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0.32
          },
          {
            _id: '8',
            _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0.32
          },
          {
            _id: '9',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0.32
          }
        ],
        RESULT_LENGTH: 8
      })
    })
})

test('SCORE TFIDF JSON all fields as default', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        AND: ['tesla']
      },
      {
        SCORE: {},
        SORT: true
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '2',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: 0.64
          },
          {
            _id: '6',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: 0.64
          },
          {
            _id: '7',
            _match: [
              { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: 0.64
          },
          {
            _id: '0',
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0.32
          },
          {
            _id: '3',
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0.32
          },
          {
            _id: '5',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0.32
          },
          {
            _id: '8',
            _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0.32
          },
          {
            _id: '9',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0.32
          }
        ],
        RESULT_LENGTH: 8
      })
    })
})

test('SCORE TFIDF JSON when using ALL_DOCUMENTS', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        ALL_DOCUMENTS: -1
      },
      {
        SCORE: {
          TYPE: 'TFIDF'
        },
        SORT: true
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '0',
            _doc: {
              _id: '0',
              make: 'Tesla',
              manufacturer: 'Volvo',
              brand: 'Volvo',
              price: 3000
            },
            _score: 0
          },
          {
            _id: '1',
            _doc: {
              _id: '1',
              make: 'BMW',
              manufacturer: 'Volvo',
              brand: 'Volvo',
              price: 12000
            },
            _score: 0
          },
          {
            _id: '2',
            _doc: {
              _id: '2',
              make: 'Tesla',
              manufacturer: 'Tesla',
              brand: 'Volvo',
              price: 14000
            },
            _score: 0
          },
          {
            _id: '3',
            _doc: {
              _id: '3',
              make: 'Tesla',
              manufacturer: 'Volvo',
              brand: 'BMW',
              price: 140000
            },
            _score: 0
          },
          {
            _id: '4',
            _doc: {
              _id: '4',
              make: 'Volvo',
              manufacturer: 'Volvo',
              brand: 'Volvo',
              price: 1000
            },
            _score: 0
          },
          {
            _id: '5',
            _doc: {
              _id: '5',
              make: 'Volvo',
              manufacturer: 'Tesla',
              brand: 'Volvo',
              price: 2000
            },
            _score: 0
          },
          {
            _id: '6',
            _doc: {
              _id: '6',
              make: 'Tesla',
              manufacturer: 'Tesla',
              brand: 'BMW',
              price: 500
            },
            _score: 0
          },
          {
            _id: '7',
            _doc: {
              _id: '7',
              make: 'BMW',
              manufacturer: 'Tesla',
              brand: 'Tesla',
              price: 5000
            },
            _score: 0
          },
          {
            _id: '8',
            _doc: {
              _id: '8',
              make: 'Volvo',
              manufacturer: 'BMW',
              brand: 'Tesla',
              price: 100
            },
            _score: 0
          },
          {
            _id: '9',
            _doc: {
              _id: '9',
              make: 'BMW',
              manufacturer: 'Tesla',
              brand: 'Volvo',
              price: 1000
            },
            _score: 0
          }
        ],
        RESULT_LENGTH: 10
      })
    })
})

test('SCORE VALUE JSON when using ALL_DOCUMENTS', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        ALL_DOCUMENTS: -1
      },
      {
        SCORE: {
          TYPE: 'VALUE'
        },
        SORT: true
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '0',
            _doc: {
              _id: '0',
              make: 'Tesla',
              manufacturer: 'Volvo',
              brand: 'Volvo',
              price: 3000
            },
            _score: ''
          },
          {
            _id: '1',
            _doc: {
              _id: '1',
              make: 'BMW',
              manufacturer: 'Volvo',
              brand: 'Volvo',
              price: 12000
            },
            _score: ''
          },
          {
            _id: '2',
            _doc: {
              _id: '2',
              make: 'Tesla',
              manufacturer: 'Tesla',
              brand: 'Volvo',
              price: 14000
            },
            _score: ''
          },
          {
            _id: '3',
            _doc: {
              _id: '3',
              make: 'Tesla',
              manufacturer: 'Volvo',
              brand: 'BMW',
              price: 140000
            },
            _score: ''
          },
          {
            _id: '4',
            _doc: {
              _id: '4',
              make: 'Volvo',
              manufacturer: 'Volvo',
              brand: 'Volvo',
              price: 1000
            },
            _score: ''
          },
          {
            _id: '5',
            _doc: {
              _id: '5',
              make: 'Volvo',
              manufacturer: 'Tesla',
              brand: 'Volvo',
              price: 2000
            },
            _score: ''
          },
          {
            _id: '6',
            _doc: {
              _id: '6',
              make: 'Tesla',
              manufacturer: 'Tesla',
              brand: 'BMW',
              price: 500
            },
            _score: ''
          },
          {
            _id: '7',
            _doc: {
              _id: '7',
              make: 'BMW',
              manufacturer: 'Tesla',
              brand: 'Tesla',
              price: 5000
            },
            _score: ''
          },
          {
            _id: '8',
            _doc: {
              _id: '8',
              make: 'Volvo',
              manufacturer: 'BMW',
              brand: 'Tesla',
              price: 100
            },
            _score: ''
          },
          {
            _id: '9',
            _doc: {
              _id: '9',
              make: 'BMW',
              manufacturer: 'Tesla',
              brand: 'Volvo',
              price: 1000
            },
            _score: ''
          }
        ],
        RESULT_LENGTH: 10
      })
    })
})

test('SCORE TFIDF JSON specified fields', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        AND: ['tesla']
      },
      {
        SCORE: {
          TYPE: 'TFIDF',
          FIELDS: ['make']
        },
        SORT: true
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '0',
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0.32
          },
          {
            _id: '2',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: 0.32
          },
          {
            _id: '3',
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0.32
          },
          {
            _id: '6',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: 0.32
          },
          {
            _id: '5',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0
          },
          {
            _id: '7',
            _match: [
              { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: 0
          },
          {
            _id: '8',
            _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0
          },
          {
            _id: '9',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 0
          }
        ],
        RESULT_LENGTH: 8
      })
    })
})

test('SCORE SUM JSON', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        AND: ['tesla']
      },
      {
        SCORE: {
          TYPE: 'SUM'
        },
        SORT: true
      }
    )
    .then(({ RESULT }) => {
      t.deepEqual(RESULT, [
        {
          _id: '2',
          _match: [
            { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
          ],
          _score: 2
        },
        {
          _id: '6',
          _match: [
            { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
          ],
          _score: 2
        },
        {
          _id: '7',
          _match: [
            { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
          ],
          _score: 2
        },
        {
          _id: '0',
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 1
        },
        {
          _id: '3',
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 1
        },
        {
          _id: '5',
          _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 1
        },
        {
          _id: '8',
          _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 1
        },
        {
          _id: '9',
          _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 1
        }
      ])
    })
})

test('SCORE SUM JSON specified fields', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        AND: ['tesla']
      },
      {
        SCORE: {
          TYPE: 'SUM',
          FIELDS: ['brand', 'manufacturer']
        },
        SORT: true
      }
    )
    .then(({ RESULT }) => {
      t.deepEqual(RESULT, [
        {
          _id: '7',
          _match: [
            { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
          ],
          _score: 2
        },
        {
          _id: '2',
          _match: [
            { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
          ],
          _score: 1
        },
        {
          _id: '5',
          _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 1
        },
        {
          _id: '6',
          _match: [
            { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
          ],
          _score: 1
        },
        {
          _id: '8',
          _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 1
        },
        {
          _id: '9',
          _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 1
        },
        {
          _id: '0',
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 0
        },
        {
          _id: '3',
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 0
        }
      ])
    })
})

test('SCORE PRODUCT JSON', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        AND: ['tesla']
      },
      {
        SCORE: {
          TYPE: 'PRODUCT'
        },
        SORT: true
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '0',
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 1
          },
          {
            _id: '2',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: 1
          },
          {
            _id: '3',
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 1
          },
          {
            _id: '5',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 1
          },
          {
            _id: '6',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: 1
          },
          {
            _id: '7',
            _match: [
              { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: 1
          },
          {
            _id: '8',
            _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 1
          },
          {
            _id: '9',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
            _score: 1
          }
        ],
        RESULT_LENGTH: 8
      })
    })
})

test('SCORE CONCAT JSON', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        AND: ['tesla']
      },
      {
        SCORE: {
          TYPE: 'CONCAT'
        },
        SORT: {
          TYPE: 'ALPHABETIC',
          DIRECTION: 'DESCENDING'
        }
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '2',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: '1.001.00'
          },
          {
            _id: '6',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: '1.001.00'
          },
          {
            _id: '7',
            _match: [
              { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }
            ],
            _score: '1.001.00'
          },
          {
            _id: '0',
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
            _score: '1.00'
          },
          {
            _id: '3',
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }],
            _score: '1.00'
          },
          {
            _id: '5',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
            _score: '1.00'
          },
          {
            _id: '8',
            _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
            _score: '1.00'
          },
          {
            _id: '9',
            _match: [{ FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' }],
            _score: '1.00'
          }
        ],
        RESULT_LENGTH: 8
      })
    })
})

test('dump "price" into SCORE', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        AND: ['tesla', { FIELD: 'price' }]
      },
      {
        SCORE: {
          TYPE: 'VALUE',
          FIELDS: ['price']
        },
        SORT: true
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '3',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'price', VALUE: 140000, SCORE: 140000 }
            ],
            _score: '140000'
          },
          {
            _id: '2',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'price', VALUE: 14000, SCORE: 14000 }
            ],
            _score: '14000'
          },
          {
            _id: '7',
            _match: [
              { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'price', VALUE: 5000, SCORE: 5000 }
            ],
            _score: '5000'
          },
          {
            _id: '0',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'price', VALUE: 3000, SCORE: 3000 }
            ],
            _score: '3000'
          },
          {
            _id: '5',
            _match: [
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'price', VALUE: 2000, SCORE: 2000 }
            ],
            _score: '2000'
          },
          {
            _id: '9',
            _match: [
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'price', VALUE: 1000, SCORE: 1000 }
            ],
            _score: '1000'
          },
          {
            _id: '6',
            _match: [
              { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'manufacturer', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'price', VALUE: 500, SCORE: 500 }
            ],
            _score: '500'
          },
          {
            _id: '8',
            _match: [
              { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
              { FIELD: 'price', VALUE: 100, SCORE: 100 }
            ],
            _score: '100'
          }
        ],
        RESULT_LENGTH: 8
      })
    })
})
