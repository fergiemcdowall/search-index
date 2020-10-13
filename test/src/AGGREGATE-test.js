import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_AGGREGATE'

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

test('simple AGGREGATE', t => {
  const { _AND, _BUCKETS, _AGGREGATE } = global[indexName]
  t.plan(1)
  _AGGREGATE({
    BUCKETS: _BUCKETS({
      FIELD: 'make',
      VALUE: {
        GTE: 'a',
        LTE: 'u'
      }
    }),
    QUERY: _AND('make:bmw')
  }).then(res => {
    t.deepEqual(res, {
      BUCKETS: [
        { FIELD: [ 'make' ], VALUE: { GTE: 'a', LTE: 'u' }, _id: [
          '1', '7', '9'
        ] }
      ],
      FACETS: [],
      RESULT: [
        { _id: '1', _match: [ 'make:bmw#1.00' ] },
        { _id: '7', _match: [ 'make:bmw#1.00' ] },
        { _id: '9', _match: [ 'make:bmw#1.00' ] }
      ]
    })
  })
})

test('simple AGGREGATE (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    AGGREGATE: {
      BUCKETS: [
        {
          FIELD: 'make',
          VALUE: {
            GTE: 'a',
            LTE: 'u'
          }
        }
      ],
      QUERY: { AND: [ 'make:bmw' ] }
    }
  }).then(res => {
    t.deepEqual(res, {
      BUCKETS: [
        {
          FIELD: [ 'make' ], VALUE: { GTE: 'a', LTE: 'u' }, _id: [
          '1', '7', '9'
          ]
        }
      ],
      FACETS: [],
      RESULT: [
        { _id: '1', _match: [ 'make:bmw#1.00' ] },
        { _id: '7', _match: [ 'make:bmw#1.00' ] },
        { _id: '9', _match: [ 'make:bmw#1.00' ] }
      ]
    })
  })
})

test('simple AGGREGATE (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    AGGREGATE: {
      BUCKETS: [{
        FIELD: 'make',
        VALUE: {
          GTE: 'a',
          LTE: 'u'
        }
      }],
      QUERY: { AND: [ 'make:bmw', 'manufacturer:tesla' ]}
    }
  }).then(res => {
    t.deepEqual(res, {
      BUCKETS: [
        {
          FIELD: [ 'make' ], VALUE: { GTE: 'a', LTE: 'u' }, _id: [
            '7', '9'
          ]
        }
      ],
      FACETS: [],
      RESULT: [
        { _id: '7', _match: [
          'make:bmw#1.00', 'manufacturer:tesla#1.00' ] },
        { _id: '9', _match: [
          'make:bmw#1.00', 'manufacturer:tesla#1.00' ] }
      ]
    })
  })
})

test('simple AGGREGATE (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    AGGREGATE: {
      BUCKETS: [{
        FIELD: 'make',
        VALUE: {
          GTE: 'a',
          LTE: 'u'
        }
      }],
      QUERY: { OR: [ 'brand:tesla', 'manufacturer:tesla' ]}
    }
  }).then(res => {
    t.deepEqual(res, {
      BUCKETS: [
        { FIELD: [ 'make' ], VALUE: { GTE: 'a', LTE: 'u' }, _id: [ '2', '6', '7', '9' ] } ],
      FACETS: [],
      RESULT: [
        { _id: '2', _match: [ 'manufacturer:tesla#1.00' ] },
        { _id: '5', _match: [ 'manufacturer:tesla#1.00' ] },
        { _id: '6', _match: [ 'manufacturer:tesla#1.00' ] },
        { _id: '7', _match: [ 'brand:tesla#1.00', 'manufacturer:tesla#1.00' ] },
        { _id: '8', _match: [ 'brand:tesla#1.00' ] },
        { _id: '9', _match: [ 'manufacturer:tesla#1.00' ] }
      ]
    })
  })
})

test('simple _AGGREGATE, using _DISTINCT', t => {
  const { _BUCKET, _GET, _FACETS, _AGGREGATE } = global[indexName]
  t.plan(1)
  _AGGREGATE({
    FACETS: _FACETS({
      FIELD: 'make',
      VALUE: {
        GTE: 'a',
        LTE: 'u'
      }
    }),
    QUERY: _GET('make:bmw')
  }).then(res => {
    t.deepEqual(res, {
      BUCKETS: [],
      FACETS: [
        { FIELD: 'make', VALUE: 'bmw', _id: [ '1', '7', '9' ] },
        { FIELD: 'make', VALUE: 'tesla', _id: [] }
      ],
      RESULT: [
        { _id: '1', _match: [ 'make:bmw#1.00' ] },
        { _id: '7', _match: [ 'make:bmw#1.00' ] },
        { _id: '9', _match: [ 'make:bmw#1.00' ] }
      ]
    })
  })
})

test('simple AGGREGATE, using DISTINCT (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    AGGREGATE: {
      FACETS: {
        FIELD: 'make',
        VALUE: {
          GTE: 'a',
          LTE: 'u'
        }
      },
      QUERY: {
        GET: {
          FIELD: [ 'make' ],
          VALUE: {
            GTE: 'a',
            LTE: 'c'
          }
        }
      }
    }
  }).then(res => {
    t.deepEqual(res, {
      BUCKETS: [],
      FACETS: [
        { FIELD: 'make', VALUE: 'bmw', _id: [ '1', '7', '9' ] },
        { FIELD: 'make', VALUE: 'tesla', _id: [] }
      ],
      RESULT: [
        { _id: '1', _match: [ 'make:bmw#1.00' ] },
        { _id: '7', _match: [ 'make:bmw#1.00' ] },
        { _id: '9', _match: [ 'make:bmw#1.00' ] }
      ]
    })
  })
})


test('simple AGGREGATE, using DISTINCT (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    AGGREGATE: {
      FACETS: {
        FIELD: 'make',
        VALUE: {
          GTE: 'a',
          LTE: 'u'
        }
      },
      QUERY: {
        GET: {
          FIELD: 'make',
          VALUE: 'bmw'
        }
      }
    }
  }).then(res => {
    t.deepEqual(res, {
      BUCKETS: [],
      FACETS: [
        { FIELD: 'make', VALUE: 'bmw', _id: [ '1', '7', '9' ] },
        { FIELD: 'make', VALUE: 'tesla', _id: [] }
      ],
      RESULT: [
        { _id: '1', _match: [ 'make:bmw#1.00' ] },
        { _id: '7', _match: [ 'make:bmw#1.00' ] },
        { _id: '9', _match: [ 'make:bmw#1.00' ] }
      ]
    })
  })
})

test('simple AGGREGATE, using DISTINCT (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    AGGREGATE: {
      FACETS: {
        FIELD: 'make',
        VALUE: {
          GTE: 'a',
          LTE: 'u'
        }
      },
      QUERY: {
        GET: 'make:bmw'
      }
    }
  }).then(res => {
    t.deepEqual(res, {
      BUCKETS: [],
      FACETS: [
        { FIELD: 'make', VALUE: 'bmw', _id: [ '1', '7', '9' ] },
        { FIELD: 'make', VALUE: 'tesla', _id: [] }
      ],
      RESULT: [
        { _id: '1', _match: [ 'make:bmw#1.00' ] },
        { _id: '7', _match: [ 'make:bmw#1.00' ] },
        { _id: '9', _match: [ 'make:bmw#1.00' ] }
      ]
    })
  })
})

