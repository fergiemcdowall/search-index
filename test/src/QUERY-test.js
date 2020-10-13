import si from '../../dist/search-index.esm.js'
import test from 'tape'

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
      "_id": 0,
      "make": "Tesla",
      "manufacturer": "Volvo",
      "brand": "Volvo"
    },
    {
      "_id": 1,
      "make": "BMW",
      "manufacturer": "Volvo",
      "brand": "Volvo"
    },
    {
      "_id": 2,
      "make": "Tesla",
      "manufacturer": "Tesla",
      "brand": "Volvo"
    },
    {
      "_id": 3,
      "make": "Tesla",
      "manufacturer": "Volvo",
      "brand": "BMW"
    },
    {
      "_id": 4,
      "make": "Volvo",
      "manufacturer": "Volvo",
      "brand": "Volvo"
    },
    {
      "_id": 5,
      "make": "Volvo",
      "manufacturer": "Tesla",
      "brand": "Volvo"
    },
    {
      "_id": 6,
      "make": "Tesla",
      "manufacturer": "Tesla",
      "brand": "BMW"
    },
    {
      "_id": 7,
      "make": "BMW",
      "manufacturer": "Tesla",
      "brand": "Tesla"
    },
    {
      "_id": 8,
      "make": "Volvo",
      "manufacturer": "BMW",
      "brand": "Tesla"
    },
    {
      "_id": 9,
      "make": "BMW",
      "manufacturer": "Tesla",
      "brand": "Volvo"
    }
  ]

  t.plan(1)
  global[indexName]._PUT(data).then(t.pass)
})

// AND
test('simple AND with 2 clauses', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    AND: [ 'make:volvo', 'manufacturer:bmw' ]
  }).then(res => {
    t.deepEqual(res, [
      { _id: '8', _match: [ 'make:volvo#1.00', 'manufacturer:bmw#1.00' ] }
    ])
  })
})

// BUCKET
test('simple BUCKET', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    BUCKETS: [
      {
        FIELD: 'make',
        VALUE: 'volvo'
      }
    ]
  }).then(res => {
    t.deepEqual(res, [
      {
        FIELD: [ 'make' ], VALUE: { GTE: 'volvo', LTE: 'volvo' }, _id: [ '4', '5', '8' ]
      }
    ])
  })
})

// AGGREGATE
test('simple AGGREGATE', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    AGGREGATE: {
      BUCKETS: [
        {
          FIELD: 'make',
          VALUE: 'volvo'        
        }
      ],
      QUERY: {
        GET: 'brand:tesla'
      }
    }
  }).then(res => {
    t.deepEqual(res, {
      BUCKETS: [
        { FIELD: [ 'make' ], VALUE: { GTE: 'volvo', LTE: 'volvo' }, _id: [ '8' ] } ],
      FACETS: [],
      RESULT: [
        { _id: '7', _match: [ 'brand:tesla#1.00' ] },
        { _id: '8', _match: [ 'brand:tesla#1.00' ] }
      ]
    })
  })
})

// DISTINCT
test('DISTINCT', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    DISTINCT: [{
      FIELD: 'make'
    }]
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'make', VALUE: 'bmw' },
      { FIELD: 'make', VALUE: 'tesla' },
      { FIELD: 'make', VALUE: 'volvo' }
    ])
  })
})

// DISTINCT and BUCKETFILTER
test('FACETS and AGGREGATE', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    AGGREGATE: {
      FACETS: {
        FIELD: 'make'
      },
      QUERY: {
        GET: {
          FIELD: 'brand',
          VALUE: 'tesla'
        }
      }
    }
  }).then(res => {
    t.deepEqual(res, {
      BUCKETS: [],
      FACETS: [
        { FIELD: 'make', VALUE: 'bmw', _id: [ '7' ] },
        { FIELD: 'make', VALUE: 'tesla', _id: [] },
        { FIELD: 'make', VALUE: 'volvo', _id: [ '8' ] }
      ],
      RESULT: [
        { _id: '7', _match: [ 'brand:tesla#1.00' ] },
        { _id: '8', _match: [ 'brand:tesla#1.00' ] }
      ]
    })
  })
})

// DOCUMENTS -> TODO

// QUERY
test('simple QUERY', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    GET: 'make:volvo'
  }).then(res => {
    t.deepEqual(res, [
      { _id: '4', _match: [ 'make:volvo#1.00' ] },
      { _id: '5', _match: [ 'make:volvo#1.00' ] },
      { _id: '8', _match: [ 'make:volvo#1.00' ] } 
    ])
  })
})

// NOT -> TODO
test('simple NOT', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    NOT: {
      INCLUDE: 'manufacturer:tesla',
      EXCLUDE: 'brand:volvo'
    }
  }).then(res => {
    t.deepEqual(res, [
      { _id: '6', _match: [ 'manufacturer:tesla#1.00' ] },
      { _id: '7', _match: [ 'manufacturer:tesla#1.00' ] },
    ])
  })
})

test('simple NOT with DOCUMENTS', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    NOT: {
      INCLUDE: 'manufacturer:tesla',
      EXCLUDE: 'brand:volvo'
    }
  }, { DOCUMENTS: true }).then(res => {
    t.deepEqual(res, [
      {
        _id: '6', _match: [ 'manufacturer:tesla#1.00' ], _doc: {
          _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW'
        }
      },
      {
        _id: '7', _match: [ 'manufacturer:tesla#1.00' ], _doc: {
          _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla'
        }
      }
    ])
  })
})


// OR
test('simple OR with 2 clauses', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    OR: [ 'make:volvo', 'brand:tesla' ]
  }).then(res => {
    t.deepEqual(res, [
      { _id: '4', _match: [ 'make:volvo#1.00' ] },
      { _id: '5', _match: [ 'make:volvo#1.00' ] },
      { _id: '7', _match: [ 'brand:tesla#1.00' ] },
      { _id: '8', _match: [ 'make:volvo#1.00', 'brand:tesla#1.00' ] }
    ])
  })
})

// SEARCH
test('simple SEARCH', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    SEARCH: [ 'tesla' ]    // TODO: should be able to search without a normal string?
  }).then(res => {
    t.deepEqual(res, [
      { _id: '2', _match: [ 'make:tesla#1.00', 'manufacturer:tesla#1.00' ], _score: 0.64 },
      { _id: '6', _match: [ 'make:tesla#1.00', 'manufacturer:tesla#1.00' ], _score: 0.64 },
      { _id: '7', _match: [ 'brand:tesla#1.00', 'manufacturer:tesla#1.00' ], _score: 0.64 },
      { _id: '0', _match: [ 'make:tesla#1.00' ], _score: 0.32 },
      { _id: '3', _match: [ 'make:tesla#1.00' ], _score: 0.32 },
      { _id: '5', _match: [ 'manufacturer:tesla#1.00' ], _score: 0.32 },
      { _id: '8', _match: [ 'brand:tesla#1.00' ], _score: 0.32 },
      { _id: '9', _match: [ 'manufacturer:tesla#1.00' ], _score: 0.32 }
    ])
  })
})

