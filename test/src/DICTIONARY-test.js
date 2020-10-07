import si from '../../dist/search-index.esm.js'
import test from 'tape'
import FuzzySet from 'fuzzyset'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_DICTIONARY'

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

test('simple DICTIONARY', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY({
    FIELD: ['colour']
  }).then(res => {
    t.deepEqual(res, [
      'blue',
      'red',
      'yellow'
    ])
  })
})

test('simple DICTIONARY- all entries', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY().then(res => {
    t.deepEqual(res, [
      'blue', 'bmw', 'red', 'tesla', 'volvo', 'yellow' 
    ])
  })
})

test('simple DICTIONARY- all entries (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({ DICTIONARY: {} }).then(res => {
    t.deepEqual(res, [
      'blue', 'bmw', 'red', 'tesla', 'volvo', 'yellow' 
    ])
  })
})


test('simple DICTIONARY, multiple fields', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY({
    FIELD: ['colour', 'brand']
  }).then(res => {
    t.deepEqual(res, [
      'blue', 'bmw', 'red', 'tesla', 'volvo', 'yellow' 
    ])
  })
})


test('simple DICTIONARY, multiple fields, gte', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY({
    FIELD: ['colour', 'brand'],
    VALUE: {
      GTE: 'c'
    }
  }).then(res => {
    t.deepEqual(res, [
      'red', 'tesla', 'volvo', 'yellow' 
    ])
  })
})

test('simple DICTIONARY, multiple fields, gte + lte', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY({
    FIELD: ['colour', 'brand'],
    VALUE: {
      GTE: 'c',
      LTE: 'u'
    }
  }).then(res => {
    t.deepEqual(res, [
      'red', 'tesla'
    ])
  })
})

test('simple DICTIONARY (JSON), multiple fields, gte + lte', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    DICTIONARY: {
      FIELD: ['colour', 'brand'],
      VALUE: {
        GTE: 'c',
        LTE: 'u'
      }
    }
  }).then(res => {
    t.deepEqual(res, [
      'red', 'tesla'
    ])
  })
})

test('simple DICTIONARY (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    DICTIONARY: {
      FIELD: ['colour']
    }
  }).then(res => {
    t.deepEqual(res, [
      'blue', 'red', 'yellow'
    ])
  })
})


test('simple DICTIONARY (JSON)', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY().then(res => {
    t.deepEqual(res, [
      'blue', 'bmw', 'red', 'tesla', 'volvo', 'yellow' 
    ])
  })
})

test('simple DICTIONARY (JSON)', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY('bl').then(res => {
    t.deepEqual(res, [
      'blue'
    ])
  })
})

test('FuzzySet test', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY().then(dict => {
    const fs = FuzzySet()
    dict.forEach(d => fs.add(d))
    t.deepEqual(fs.get('blux'), [ [ 0.75, 'blue' ] ])
  })
})
