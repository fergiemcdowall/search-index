import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'DISTINCT'

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
  global[indexName].PUT(data).then(t.pass)
})

test('simple DISTINCT', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    field: 'colour'
  }).then(res => {
    t.looseEqual(res, [
      { field: 'colour', value: 'blue' },
      { field: 'colour', value: 'red' },
      { field: 'colour', value: 'yellow' }
    ])
  })
})


test('simple DISTINCT with range', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    field: 'colour',
    value: {
      gte: 'a',
      lte: 'c'
    }
  }).then(res => {
    t.looseEqual(res, [
      { field: 'colour', value: 'blue' }
    ])
  })
})


test('simple DISTINCT with range', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    field: 'colour',
    value: {
      gte: 'c'
    }
  }).then(res => {
    t.looseEqual(res, [
      { field: 'colour', value: 'red' }, { field: 'colour', value: 'yellow' }
    ])
  })
})


test('simple DISTINCT with range', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    field: 'colour',
    value: {
      lte: 'c'
    }
  }).then(res => {
    t.looseEqual(res, [
      { field: 'colour', value: 'blue' }
    ])
  })
})

