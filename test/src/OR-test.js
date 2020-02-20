import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'OR'

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


test('simple OR with 1 clause', t => {
  t.plan(1)
  global[indexName].OR(
    'make:Volvo'
  ).then(res => {
    t.looseEqual(res, [
      { _id: '4', _match: [ 'make:Volvo#1.00' ] },
      { _id: '5', _match: [ 'make:Volvo#1.00' ] },
      { _id: '8', _match: [ 'make:Volvo#1.00' ] } 
    ])
  })
})

test('simple OR with 2 clauses', t => {
  t.plan(1)
  global[indexName].OR(
    'brand:Volvo', 'brand:Tesla'
  ).then(res => {
    t.looseEqual(res, [
      { _id: '0', _match: [ 'brand:Volvo#1.00' ] },
      { _id: '1', _match: [ 'brand:Volvo#1.00' ] },
      { _id: '2', _match: [ 'brand:Volvo#1.00' ] },
      { _id: '4', _match: [ 'brand:Volvo#1.00' ] },
      { _id: '5', _match: [ 'brand:Volvo#1.00' ] },
      { _id: '7', _match: [ 'brand:Tesla#1.00' ] },
      { _id: '8', _match: [ 'brand:Tesla#1.00' ] },
      { _id: '9', _match: [ 'brand:Volvo#1.00' ] } 
    ])
  })
})

test('simple OR with 2 clauses', t => {
  t.plan(1)
  global[indexName].OR(
    'brand:Volvo', 'manufacturer:Tesla'
  ).then(res => {
    t.looseEqual(res, [
      { _id: '0', _match: [ 'brand:Volvo#1.00' ] },
      { _id: '1', _match: [ 'brand:Volvo#1.00' ] },
      { _id: '2', _match: [ 'brand:Volvo#1.00', 'manufacturer:Tesla#1.00' ] },
      { _id: '4', _match: [ 'brand:Volvo#1.00' ] },
      { _id: '5', _match: [ 'brand:Volvo#1.00', 'manufacturer:Tesla#1.00' ] },
      { _id: '6', _match: [ 'manufacturer:Tesla#1.00' ] },
      { _id: '7', _match: [ 'manufacturer:Tesla#1.00' ] },
      { _id: '9', _match: [ 'brand:Volvo#1.00', 'manufacturer:Tesla#1.00' ] } 
    ])
  })
})


test('simple OR with 2 clauses (embedded AND)', t => {
  const { OR, AND } = global[indexName]
  t.plan(1)
  OR(
    AND('brand:Volvo', 'manufacturer:Tesla'),
    'make:BMW'
  ).then(res => {
    t.looseEqual(res, [
      { _id: '1', _match: [ 'make:BMW#1.00' ] },
      { _id: '2', _match: [ 'brand:Volvo#1.00', 'manufacturer:Tesla#1.00' ] },
      { _id: '5', _match: [ 'brand:Volvo#1.00', 'manufacturer:Tesla#1.00' ] },
      { _id: '7', _match: [ 'make:BMW#1.00' ] },
      { _id: '9', _match: [ 'brand:Volvo#1.00', 'manufacturer:Tesla#1.00', 'make:BMW#1.00' ] } 
    ])
  })
})


