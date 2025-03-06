import test from 'tape'

import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_AND'
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

test('simple AND with 1 clause', t => {
  t.plan(1)
  global[indexName]._AND(['make:volvo']).then(res => {
    t.deepEqual(res, [
      { _id: '4', _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] },
      { _id: '5', _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] },
      { _id: '8', _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] }
    ])
  })
})

test('simple _AND with 1 clause', t => {
  t.plan(1)
  global[indexName]._AND(['manufacturer:bmw']).then(res => {
    t.deepEqual(res, [
      {
        _id: '8',
        _match: [{ FIELD: 'manufacturer', VALUE: 'bmw', SCORE: '1.00' }]
      }
    ])
  })
})

test('simple _AND with 2 clauses', t => {
  t.plan(1)
  global[indexName]._AND(['make:volvo', 'manufacturer:bmw']).then(res => {
    t.deepEqual(res, [
      {
        _id: '8',
        _match: [
          { FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' },
          { FIELD: 'manufacturer', VALUE: 'bmw', SCORE: '1.00' }
        ]
      }
    ])
  })
})

test('simple AND with 2 clauses (JSON)', t => {
  t.plan(1)
  const q = {
    AND: ['make:volvo', 'manufacturer:bmw']
  }
  global[indexName].QUERY(q).then(res => {
    t.deepEqual(res, {
      QUERY: q,
      OPTIONS: {},
      RESULT: [
        {
          _id: '8',
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

test('simple AND with 2 clauses (JSON)', t => {
  t.plan(1)
  const q = {
    AND: ['volvo', 'bmw']
  }
  global[indexName].QUERY(q).then(res => {
    t.deepEqual(res, {
      QUERY: q,
      OPTIONS: {},
      RESULT: [
        {
          _id: '1',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'volvo', SCORE: '1.00' }
          ]
        },
        {
          _id: '9',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }
          ]
        },
        {
          _id: '8',
          _match: [
            { FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'bmw', SCORE: '1.00' }
          ]
        },
        {
          _id: '3',
          _match: [
            { FIELD: 'brand', VALUE: 'bmw', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'volvo', SCORE: '1.00' }
          ]
        }
      ],
      RESULT_LENGTH: 4,
      PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
    })
  })
})

test('_AND with no VALUE', t => {
  t.plan(1)
  global[indexName]
    ._AND([
      {
        FIELD: ['make']
      }
    ])
    .then(res => {
      t.deepEqual(res, [
        {
          _id: '1',
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
        },
        {
          _id: '7',
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
        },
        {
          _id: '9',
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
        },
        {
          _id: '0',
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: '2',
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: '3',
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: '6',
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: '4',
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: '5',
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: '8',
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        }
      ])
    })
})

test('AND with no VALUE (JSON)', t => {
  t.plan(1)
  const q = {
    AND: [
      {
        FIELD: ['make']
      }
    ]
  }
  global[indexName].QUERY(q).then(res => {
    t.deepEqual(res, {
      QUERY: q,
      OPTIONS: {},
      RESULT: [
        {
          _id: '1',
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
        },
        {
          _id: '7',
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
        },
        {
          _id: '9',
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
        },
        {
          _id: '0',
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: '2',
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: '3',
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: '6',
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: '4',
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: '5',
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: '8',
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        }
      ],
      RESULT_LENGTH: 10,
      PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
    })
  })
})

test('AND with nested OR (JSON)', t => {
  t.plan(1)
  const { QUERY } = global[indexName]
  const q = {
    AND: [
      'brand:volvo',
      {
        OR: ['make:bmw', 'make:volvo']
      }
    ]
  }
  QUERY(q).then(res => {
    t.deepEqual(res, {
      QUERY: {
        AND: [
          'brand:volvo',
          {
            OR: ['make:bmw', 'make:volvo']
          }
        ]
      },
      OPTIONS: {},
      RESULT: [
        {
          _id: '1',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }
          ]
        },
        {
          _id: '4',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }
          ]
        },
        {
          _id: '5',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }
          ]
        },
        {
          _id: '9',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }
          ]
        }
      ],
      RESULT_LENGTH: 4,
      PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
    })
  })
})
