import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'SORT'

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



test('SORT ALPHABETIC DESCENDING', t => {
  t.plan(1)
  global[indexName].DOCUMENTS().then(
    docs => global[indexName].SORT(docs, {
      type: 'ALPHABETIC',
      direction: 'DESCENDING',
      field: '_doc.make'
    })
  ).then(res => {
    t.deepEqual(res, [

      { _id: 4, _doc: { _id: 4, make: 'Volvo', manufacturer: 'Volvo', brand: 'Volvo', price: 1000 } },
      { _id: 5, _doc: { _id: 5, make: 'Volvo', manufacturer: 'Tesla', brand: 'Volvo', price: 2000 } },
      { _id: 8, _doc: { _id: 8, make: 'Volvo', manufacturer: 'BMW', brand: 'Tesla', price: 100 } },
      { _id: 0, _doc: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo', price: 3000 } },
      { _id: 2, _doc: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo', price: 14000 } },
      { _id: 3, _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW', price: 140000 } },
      { _id: 6, _doc: { _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW', price: 500 } },
      { _id: 1, _doc: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo', price: 12000 } },
      { _id: 7, _doc: { _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla', price: 5000 } },
      { _id: 9, _doc: { _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo', price: 1000 } }
      
    ])
  })
})

test('SORT ALPHABETIC ASCENDING', t => {
  t.plan(1)
  global[indexName].DOCUMENTS().then(
    docs => global[indexName].SORT(docs, {
      type: 'ALPHABETIC',
      direction: 'ASCENDING',
      field: '_doc.make'
    })
  ).then(res => {
    t.deepEqual(res, [
      { _id: 1, _doc: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo', price: 12000 } },
      { _id: 7, _doc: { _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla', price: 5000 } },
      { _id: 9, _doc: { _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo', price: 1000 } },
      { _id: 0, _doc: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo', price: 3000 } },
      { _id: 2, _doc: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo', price: 14000 } },
      { _id: 3, _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW', price: 140000 } },
      { _id: 6, _doc: { _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW', price: 500 } },
      { _id: 4, _doc: { _id: 4, make: 'Volvo', manufacturer: 'Volvo', brand: 'Volvo', price: 1000 } },
      { _id: 5, _doc: { _id: 5, make: 'Volvo', manufacturer: 'Tesla', brand: 'Volvo', price: 2000 } },
      { _id: 8, _doc: { _id: 8, make: 'Volvo', manufacturer: 'BMW', brand: 'Tesla', price: 100 } } 
    ])
  })
})

test('SORT NUMERIC ASCENDING', t => {
  t.plan(1)
  global[indexName].DOCUMENTS().then(
    docs => global[indexName].SORT(docs, {
      type: 'NUMERIC',
      direction: 'ASCENDING',
      field: '_doc.price'
    })
  ).then(res => {
    t.deepEqual(res, [
      { _id: 8, _doc: { _id: 8, make: 'Volvo', manufacturer: 'BMW', brand: 'Tesla', price: 100 } },
      { _id: 6, _doc: { _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW', price: 500 } },
      { _id: 4, _doc: { _id: 4, make: 'Volvo', manufacturer: 'Volvo', brand: 'Volvo', price: 1000 } },
      { _id: 9, _doc: { _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo', price: 1000 } },
      { _id: 5, _doc: { _id: 5, make: 'Volvo', manufacturer: 'Tesla', brand: 'Volvo', price: 2000 } },
      { _id: 0, _doc: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo', price: 3000 } },
      { _id: 7, _doc: { _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla', price: 5000 } },
      { _id: 1, _doc: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo', price: 12000 } },
      { _id: 2, _doc: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo', price: 14000 } },
      { _id: 3, _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW', price: 140000 } }
    ])
  })
})


test('SORT NUMERIC DESCENDING', t => {
  t.plan(1)
  global[indexName].DOCUMENTS().then(
    docs => global[indexName].SORT(docs, {
      type: 'NUMERIC',
      direction: 'DESCENDING',
      field: '_doc.price'
    })
  ).then(res => {
    t.deepEqual(res, [
      { _id: 3, _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW', price: 140000 } },
      { _id: 2, _doc: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo', price: 14000 } },
      { _id: 1, _doc: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo', price: 12000 } },
      { _id: 7, _doc: { _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla', price: 5000 } },
      { _id: 0, _doc: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo', price: 3000 } },
      { _id: 5, _doc: { _id: 5, make: 'Volvo', manufacturer: 'Tesla', brand: 'Volvo', price: 2000 } },
      { _id: 4, _doc: { _id: 4, make: 'Volvo', manufacturer: 'Volvo', brand: 'Volvo', price: 1000 } },
      { _id: 9, _doc: { _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo', price: 1000 } },
      { _id: 6, _doc: { _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW', price: 500 } },
      { _id: 8, _doc: { _id: 8, make: 'Volvo', manufacturer: 'BMW', brand: 'Tesla', price: 100 } }
    ])
  })
})


test('SORT NUMERIC DESCENDING (JSON)', t => {
  t.plan(1)
  global[indexName].QUERY({
    DOCUMENTS: true
  }, {
    SORT: {
      type: 'NUMERIC',
      direction: 'DESCENDING',
      field: '_doc.price'
    }
  }).then(res => {
    t.deepEqual(res, [
      { _id: 3, _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW', price: 140000 } },
      { _id: 2, _doc: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo', price: 14000 } },
      { _id: 1, _doc: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo', price: 12000 } },
      { _id: 7, _doc: { _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla', price: 5000 } },
      { _id: 0, _doc: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo', price: 3000 } },
      { _id: 5, _doc: { _id: 5, make: 'Volvo', manufacturer: 'Tesla', brand: 'Volvo', price: 2000 } },
      { _id: 4, _doc: { _id: 4, make: 'Volvo', manufacturer: 'Volvo', brand: 'Volvo', price: 1000 } },
      { _id: 9, _doc: { _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo', price: 1000 } },
      { _id: 6, _doc: { _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW', price: 500 } },
      { _id: 8, _doc: { _id: 8, make: 'Volvo', manufacturer: 'BMW', brand: 'Tesla', price: 100 } }
    ])
  })
})


