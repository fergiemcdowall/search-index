import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'PAGE'

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



test('get page 2 (called "1": count from "0") with page size of 3', t => {
  t.plan(1)
  global[indexName].DOCUMENTS().then(
    docs => global[indexName].PAGE(docs, {
      NUMBER: 1,
      SIZE: 3
    })
  ).then(res => {
    t.looseEqual(res, [
      { _id: 3, _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW' } },
      { _id: 4, _doc: { _id: 4, make: 'Volvo', manufacturer: 'Volvo', brand: 'Volvo' } },
      { _id: 5, _doc: { _id: 5, make: 'Volvo', manufacturer: 'Tesla', brand: 'Volvo' } } 
    ])
  })
})


test('get page 2 (called "1": count from "0") with page size of 3 (JSON)', t => {
  t.plan(1)
  global[indexName].QUERY({
    DOCUMENTS: true
  }, {
    PAGE: {
      NUMBER: 1,
      SIZE: 3
    }
  }).then(res => {
    t.looseEqual(res, [
      { _id: 3, _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW' } },
      { _id: 4, _doc: { _id: 4, make: 'Volvo', manufacturer: 'Volvo', brand: 'Volvo' } },
      { _id: 5, _doc: { _id: 5, make: 'Volvo', manufacturer: 'Tesla', brand: 'Volvo' } } 
    ])
  })
})


test('get last page with page size of 4', t => {
  t.plan(1)
  global[indexName].DOCUMENTS().then(
    docs => global[indexName].PAGE(docs, {
      NUMBER: -1,
      SIZE: 4
    })
  ).then(res => {
    t.looseEqual(res, [
      { _id: 6, _doc: { _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW' } },
      { _id: 7, _doc: { _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla' } },
      { _id: 8, _doc: { _id: 8, make: 'Volvo', manufacturer: 'BMW', brand: 'Tesla' } },
      { _id: 9, _doc: { _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo' } }
    ])
  })
})


