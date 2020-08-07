import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'MAXMIN'

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
      "price": 3000
    },
    {
      "_id": 1,
      "make": "BMW",
      "manufacturer": "Volvo",
      "brand": "Volvo",
      "price": 12000
    },
    {
      "_id": 2,
      "make": "Tesla",
      "manufacturer": "Tesla",
      "brand": "Volvo",
      "price": 14000
    },
    {
      "_id": 3,
      "make": "Tesla",
      "manufacturer": "Volvo",
      "brand": "BMW",
      "price": 140000
    },
    {
      "_id": 4,
      "make": "Volvo",
      "manufacturer": "Volvo",
      "brand": "Volvo",
      "price": 1000
    },
    {
      "_id": 5,
      "make": "Volvo",
      "manufacturer": "Tesla",
      "brand": "Volvo",
      "price": 2000
    },
    {
      "_id": 6,
      "make": "Tesla",
      "manufacturer": "Tesla",
      "brand": "BMW",
      "price": 500
    },
    {
      "_id": 7,
      "make": "BMW",
      "manufacturer": "Tesla",
      "brand": "Tesla",
      "price": 5000
    },
    {
      "_id": 8,
      "make": "Volvo",
      "manufacturer": "BMW",
      "brand": "Tesla",
      "price": 100
    },
    {
      "_id": 9,
      "make": "BMW",
      "manufacturer": "Tesla",
      "brand": "Volvo",
      "price": 1000
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('simple _MAX', t => {
  const { _MAX } = global[indexName]
  t.plan(1)
  _MAX('price').then(count => {
    t.equals(count, '5000')
  })
})

test('simple MAX (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    MAX: 'price'
  }).then(count => {
    t.equals(count, '5000')
  })
})

test('simple _MIN', t => {
  const { _MIN } = global[indexName]
  t.plan(1)
  _MIN('price').then(count => {
    t.equals(count, '100')
  })
})

test('simple MIN (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    MIN: 'price'
  }).then(count => {
    t.equals(count, '100')
  })
})

test('simple MAX (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    MAX: 'make'
  }).then(count => {
    t.equals(count, 'volvo')
  })
})

test('simple MIN (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    MIN: 'make'
  }).then(count => {
    t.equals(count, 'bmw')
  })
})
