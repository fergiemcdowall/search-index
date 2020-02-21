import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'AND'

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


test('simple AND with 1 clause', t => {
  t.plan(1)
  global[indexName].AND(
    'make:volvo'
  ).then(res => {
    t.looseEqual(res, [
      { _id: '4', _match: [ 'make:volvo#1.00' ] },
      { _id: '5', _match: [ 'make:volvo#1.00' ] },
      { _id: '8', _match: [ 'make:volvo#1.00' ] } 
    ])
  })
})

test('simple AND with 1 clause', t => {
  t.plan(1)
  global[indexName].AND(
    'manufacturer:bmw'
  ).then(res => {
    t.looseEqual(res, [
      { _id: '8', _match: [ 'manufacturer:bmw#1.00' ] }
    ])
  })
})

test('simple AND with 2 clauses', t => {
  t.plan(1)
  global[indexName].AND(
    'make:volvo', 'manufacturer:bmw'
  ).then(res => {
    t.looseEqual(res, [
      { _id: '8', _match: [ 'make:volvo#1.00', 'manufacturer:bmw#1.00' ] }
    ])
  })
})

