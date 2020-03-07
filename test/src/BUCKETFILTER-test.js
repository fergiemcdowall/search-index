import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'BUCKETFILTER'

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
  global[indexName].PUT(data).then(t.pass)
})

test('simple BUCKETFILTER', t => {
  const { GET, BUCKET, BUCKETFILTER } = global[indexName]
  t.plan(1)
  BUCKETFILTER(
    [
      BUCKET({
        field: 'make',
        value: {
          gte: 'a',
          lte: 'u'
        }
      })
    ],
    GET('make:bmw') // TODO: this should be able to be just 'make:bmw'
  ).then(res => {
    t.looseEqual(res, [
      {
        field: 'make', value: { gte: 'a', lte: 'u' }, _id: [ '1', '7', '9' ] 
      }
    ])
  })
})

test('simple BUCKETFILTER, using DICTIONARY', t => {
  const { BUCKET, GET, DICTIONARY, BUCKETFILTER } = global[indexName]
  t.plan(1)
  BUCKETFILTER(
    [
      'make:bmw',
      'make:tesla'
    ].map(BUCKET),
    GET('make:bmw') // TODO: this should be able to be just 'make:bmw'
  ).then(res => {
    t.looseEqual(res, [
      { field: [ 'make' ], value: { gte: 'bmw', lte: 'bmw' }, _id: [ '1', '7', '9' ] },
      { field: [ 'make' ], value: { gte: 'tesla', lte: 'tesla' }, _id: [] }
    ])
  })
})


test('simple BUCKETFILTER (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    BUCKETFILTER: {
      BUCKETS: [{
        field: 'make',
        value: {
          gte: 'a',
          lte: 'u'
        }
      }],
      FILTER: { GET: 'make:bmw'}
    }
  }).then(res => {
    t.looseEqual(res, [
      {
        field: 'make', value: { gte: 'a', lte: 'u' }, _id: [ '1', '7', '9' ] 
      }
    ])
  })
})

test('simple BUCKETFILTER (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    BUCKETFILTER: {
      BUCKETS: [{
        field: 'make',
        value: {
          gte: 'a',
          lte: 'u'
        }
      }],
      FILTER: { AND: [ 'make:bmw', 'manufacturer:tesla' ]}
    }
  }).then(res => {
    t.looseEqual(res, [
      {
        field: 'make', value: { gte: 'a', lte: 'u' }, _id: [ '7', '9' ] 
      }
    ])
  })
})

test('simple BUCKETFILTER (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    BUCKETFILTER: {
      BUCKETS: [{
        field: 'make',
        value: {
          gte: 'a',
          lte: 'u'
        }
      }],
      FILTER: { OR: [ 'brand:tesla', 'manufacturer:tesla' ]}
    }
  }).then(res => {
    t.looseEqual(res, [
      {
        field: 'make', value: { gte: 'a', lte: 'u' }, _id: [ '2', '6', '7', '9' ] 
      }
    ])
  })
})


