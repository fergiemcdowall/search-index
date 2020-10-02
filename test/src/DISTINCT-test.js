import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_DISTINCT'

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
      "brand": "Volvo",
      "colour": "yellow"
    },
    {
      "_id": 1,
      "make": "BMW",
      "manufacturer": "Volvo",
      "brand": "Volvo",
      "colour": "red"
    },
    {
      "_id": 2,
      "make": "Tesla",
      "manufacturer": "Tesla",
      "brand": "Volvo",
      "colour": "blue"
    },
    {
      "_id": 3,
      "make": "Tesla",
      "manufacturer": "Volvo",
      "brand": "BMW",
      "colour": "red"
    },
    {
      "_id": 4,
      "make": "Volvo",
      "manufacturer": "Volvo",
      "brand": "Volvo",
      "colour": "red"
    },
    {
      "_id": 5,
      "make": "Volvo",
      "manufacturer": "Tesla",
      "brand": "Volvo",
      "colour": "blue"
    },
    {
      "_id": 6,
      "make": "Tesla",
      "manufacturer": "Tesla",
      "brand": "BMW",
      "colour": "yellow"
    },
    {
      "_id": 7,
      "make": "BMW",
      "manufacturer": "Tesla",
      "brand": "Tesla",
      "colour": "yellow"
    },
    {
      "_id": 8,
      "make": "Volvo",
      "manufacturer": "BMW",
      "brand": "Tesla",
      "colour": "blue"
    },
    {
      "_id": 9,
      "make": "BMW",
      "manufacturer": "Tesla",
      "brand": "Volvo",
      "colour": "red"
    }
  ]

  t.plan(1)
  global[indexName]._PUT(data).then(t.pass)
})

test('simple _DISTINCT', t => {
  const { _DISTINCT } = global[indexName]
  t.plan(1)
  _DISTINCT({
    FIELD: 'colour'
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'blue' },
      { FIELD: 'colour', VALUE: 'red' },
      { FIELD: 'colour', VALUE: 'yellow' }
    ])
  })
})


test('simple _DISTINCT with range', t => {
  const { _DISTINCT } = global[indexName]
  t.plan(1)
  _DISTINCT({
    FIELD: 'colour',
    VALUE: {
      GTE: 'a',
      LTE: 'c'
    }
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'blue' }
    ])
  })
})


test('simple _DISTINCT with range', t => {
  const { _DISTINCT } = global[indexName]
  t.plan(1)
  _DISTINCT({
    FIELD: 'colour',
    VALUE: {
      GTE: 'c'
    }
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'red' },
      { FIELD: 'colour', VALUE: 'yellow' }
    ])
  })
})


test('simple _DISTINCT with range', t => {
  const { _DISTINCT } = global[indexName]
  t.plan(1)
  _DISTINCT({
    FIELD: 'colour',
    VALUE: {
      LTE: 'c'
    }
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'blue' }
    ])
  })
})

test('simple DISTINCT', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    DISTINCT: {
      FIELD: 'colour'
    }
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'blue' },
      { FIELD: 'colour', VALUE: 'red' },
      { FIELD: 'colour', VALUE: 'yellow' }
    ])
  })
})

test('simple DISTINCT', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    DISTINCT: {
      VALUE: 'red'
    }
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'red' }
    ])
  })
})

test('simple DISTINCT', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    DISTINCT: {
      VALUE: 'volvo'
    }
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'brand', VALUE: 'volvo' },
      { FIELD: 'make', VALUE: 'volvo' },
      { FIELD: 'manufacturer', VALUE: 'volvo' } 
    ])
  })
})


test('simple DISTINCT', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    DISTINCT: {
      FIELD: 'brand'
    }
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'brand', VALUE: 'bmw' },
      { FIELD: 'brand', VALUE: 'tesla' },
      { FIELD: 'brand', VALUE: 'volvo' } 
    ])
  })
})
