import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_BUCKET'

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


test('simple _BUCKET', t => {
  const { _BUCKET } = global[indexName]
  t.plan(1)
  _BUCKET(
    'make:volvo',
  ).then(res => {
    t.deepEqual(res, {
      FIELD: [ 'make' ], VALUE: { GTE: 'volvo', LTE: 'volvo' }, _id: [ '4', '5', '8' ]
    })
  })
})

test('simple _BUCKET with a range', t => {
  const { _BUCKET } = global[indexName]
  t.plan(1)
  _BUCKET({
    FIELD: 'make',
    VALUE: {
      GTE: 'a',
      LTE: 'u'
    }
  }).then(res => {
    t.deepEqual(res, {
      FIELD: [ 'make' ], VALUE: { GTE: 'a', LTE: 'u' }, _id: [ '0', '1', '2', '3', '6', '7', '9' ] 
    })
  })
})

test('simple BUCKET (JSON)', t => {
  const { GET } = global[indexName]
  t.plan(1)
  GET({
    BUCKET: 'make:volvo'
  }).then(res => {
    t.deepEqual(res, {
      FIELD: [ 'make' ], VALUE: { GTE: 'volvo', LTE: 'volvo' }, _id: [ '4', '5', '8' ]
    })
  })
})


test('simple BUCKET with a range (JSON)', t => {
  const { GET } = global[indexName]
  t.plan(1)
  GET({
    BUCKET: {
      FIELD: 'make',
      VALUE: {
        GTE: 'a',
        LTE: 'u'
      }
    }
  }).then(res => {
    t.deepEqual(res, {
      FIELD: [ 'make' ], VALUE: { GTE: 'a', LTE: 'u' }, _id: [ '0', '1', '2', '3', '6', '7', '9' ] 
    })
  })
})


