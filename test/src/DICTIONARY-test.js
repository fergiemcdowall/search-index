import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'DICTIONARY'

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

test('simple DICTIONARY', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY({ fields: ['colour'] }).then(res => {
    t.looseEqual(res, [
      'blue',
      'red',
      'yellow'
    ])
  })
})

test('simple DICTIONARY, multiple fields', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY({ fields: ['colour', 'brand'] }).then(res => {
    t.looseEqual(res, [
      'blue', 'bmw', 'red', 'tesla', 'volvo', 'yellow' 
    ])
  })
})


test('simple DICTIONARY, multiple fields, gte', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY({
    fields: ['colour', 'brand'],
    gte: 'c'
  }).then(res => {
    t.looseEqual(res, [
      'red', 'tesla', 'volvo', 'yellow' 
    ])
  })
})

test('simple DICTIONARY, multiple fields, gte + lte', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY({
    fields: ['colour', 'brand'],
    gte: 'c',
    lte: 'u'
  }).then(res => {
    t.looseEqual(res, [
      'red', 'tesla'
    ])
  })
})


test('simple DICTIONARY (JSON), multiple fields, gte + lte', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    DICTIONARY: {
      fields: ['colour', 'brand'],
      gte: 'c',
      lte: 'u'
    }
  }).then(res => {
    t.looseEqual(res, [
      'red', 'tesla'
    ])
  })
})

test('simple DICTIONARY (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    DICTIONARY: { fields: ['colour'] }
  }).then(res => {
    t.looseEqual(res, [
      'blue',
      'red',
      'yellow'
    ])
  })
})

test('simple DICTIONARY (JSON, with fields)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    DICTIONARY: {
      fields: ['colour'],
      options: { withFieldName: true }
    }
  }).then(res => {
    t.looseEqual(res, [
      'colour:blue',
      'colour:red',
      'colour:yellow'
    ])
  })
})