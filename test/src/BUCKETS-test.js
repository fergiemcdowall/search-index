import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'BUCKETS'

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


test('simple _BUCKETS', t => {
  const { _BUCKETS } = global[indexName]
  t.plan(1)
  _BUCKETS(
    'make:volvo',
    'make:bmw',
    'make:tesla'
  ).then(res => {
    t.deepEqual(res, [
      { FIELD: [ 'make' ], VALUE: { GTE: 'volvo', LTE: 'volvo' }, _id: [ '4', '5', '8' ] },
      { FIELD: [ 'make' ], VALUE: { GTE: 'bmw', LTE: 'bmw' }, _id: [ '1', '7', '9' ] },
      { FIELD: [ 'make' ], VALUE: { GTE: 'tesla', LTE: 'tesla' }, _id: [ '0', '2', '3', '6' ] } 
    ])
  })
})

test('simple _BUCKETS', t => {
  const { _BUCKETS } = global[indexName]
  t.plan(1)
  _BUCKETS(
    { FIELD: 'make', VALUE: 'bmw' },
    { FIELD: 'make', VALUE: 'tesla' },
    { FIELD: 'make', VALUE: 'volvo' }
  ).then(res => {
    t.deepEqual(res, [
      { FIELD: [ 'make' ], VALUE: { GTE: 'bmw', LTE: 'bmw' }, _id: [ '1', '7', '9' ] },
      { FIELD: [ 'make' ], VALUE: { GTE: 'tesla', LTE: 'tesla' }, _id: [ '0', '2', '3', '6' ] }, 
      { FIELD: [ 'make' ], VALUE: { GTE: 'volvo', LTE: 'volvo' }, _id: [ '4', '5', '8' ] }
    ])
  })
})


test('simple _DISTINCT.then(_BUCKETS)', t => {
  const { _DISTINCT, _BUCKETS } = global[indexName]
  t.plan(1)
  _DISTINCT({ FIELD: 'make' }).then(
    dist => _BUCKETS(...dist)
  ).then(res => {
    t.deepEqual(res, [
      { FIELD: [ 'make' ], VALUE: { GTE: 'bmw', LTE: 'bmw' }, _id: [ '1', '7', '9' ] },
      { FIELD: [ 'make' ], VALUE: { GTE: 'tesla', LTE: 'tesla' }, _id: [ '0', '2', '3', '6' ] }, 
      { FIELD: [ 'make' ], VALUE: { GTE: 'volvo', LTE: 'volvo' }, _id: [ '4', '5', '8' ] }
    ])
  })
})


test('QUERY { BUCKETS: ... }', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    BUCKETS: [
      'make:bmw',
      'make:tesla',
      'make:volvo'
    ]
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: [ 'make' ], VALUE: { GTE: 'bmw', LTE: 'bmw' }, _id: [ '1', '7', '9' ] },
      { FIELD: [ 'make' ], VALUE: { GTE: 'tesla', LTE: 'tesla' }, _id: [ '0', '2', '3', '6' ] },
      { FIELD: [ 'make' ], VALUE: { GTE: 'volvo', LTE: 'volvo' }, _id: [ '4', '5', '8' ] }
    ])
  })
})


test('QUERY { BUCKETS: ... }', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    BUCKETS: [
      { FIELD: 'make', VALUE: 'bmw' },
      { FIELD: 'make', VALUE: 'tesla' },
      { FIELD: 'make', VALUE: 'volvo' }
    ]
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: [ 'make' ], VALUE: { GTE: 'bmw', LTE: 'bmw' }, _id: [ '1', '7', '9' ] },
      { FIELD: [ 'make' ], VALUE: { GTE: 'tesla', LTE: 'tesla' }, _id: [ '0', '2', '3', '6' ] },
      { FIELD: [ 'make' ], VALUE: { GTE: 'volvo', LTE: 'volvo' }, _id: [ '4', '5', '8' ] }
    ])
  })
})


