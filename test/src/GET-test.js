import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'GET'

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


test('simple GET', t => {
  t.plan(1)
  global[indexName].GET(
    'make:volvo'
  ).then(res => {
    t.looseEqual(res, [
      { _id: '4', _match: [ 'make:volvo#1.00' ] },
      { _id: '5', _match: [ 'make:volvo#1.00' ] },
      { _id: '8', _match: [ 'make:volvo#1.00' ] } 
    ])
  })
})

test('simple GET', t => {
  t.plan(1)
  global[indexName].GET(
    {
      field: 'make',
      value: 'volvo'
    }
  ).then(res => {
    t.looseEqual(res, [
      { _id: '4', _match: [ 'make:volvo#1.00' ] },
      { _id: '5', _match: [ 'make:volvo#1.00' ] },
      { _id: '8', _match: [ 'make:volvo#1.00' ] } 
    ])
  })
})

test('simple GET', t => {
  t.plan(1)
  global[indexName].GET(
    {
      field: 'make',
      value: {
        gte: 'a',
        lte: 'c'
      }
    }
  ).then(res => {
    t.looseEqual(res, [
      { _id: '1', _match: [ 'make:bmw#1.00' ] },
      { _id: '7', _match: [ 'make:bmw#1.00' ] },
      { _id: '9', _match: [ 'make:bmw#1.00' ] }
    ])
  })
})

test('simple GET using json with QUERY', t => {
  t.plan(1)
  global[indexName].QUERY({
    GET: {
      field: 'make',
      value: {
        gte: 'a',
        lte: 'c'
      }
    }
  }).then(res => {
    t.looseEqual(res, [
      { _id: '1', _match: [ 'make:bmw#1.00' ] },
      { _id: '7', _match: [ 'make:bmw#1.00' ] },
      { _id: '9', _match: [ 'make:bmw#1.00' ] }
    ])
  })
})