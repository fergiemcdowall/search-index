import FuzzySet from 'fuzzyset'
import test from 'tape'

import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_DICTIONARY'
const global = {}

test('create a search index', t => {
  t.plan(1)
  try {
    global[indexName] = new SearchIndex({ name: indexName })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add data', t => {
  const data = [
    {
      _id: 0,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      colour: 'yellow',
      price: 10
    },
    {
      _id: 1,
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      colour: 'red',
      price: 9
    },
    {
      _id: 2,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colour: 'blue',
      price: 10
    },
    {
      _id: 3,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW',
      colour: 'red',
      price: 1000000
    },
    {
      _id: 4,
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      colour: 'red',
      price: 0
    },
    {
      _id: 5,
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colour: 'blue',
      price: 0
    },
    {
      _id: 6,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW',
      colour: 'yellow',
      price: 99999999999
    },
    {
      _id: 7,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla',
      colour: 'yellow',
      price: 10000000000000
    },
    {
      _id: 8,
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla',
      colour: 'blue',
      price: 33
    },
    {
      _id: 9,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colour: 'red',
      price: 222
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('simple DICTIONARY', t => {
  t.plan(1)
  global[indexName]
    .DICTIONARY({
      FIELD: ['colour']
    })
    .then(res => {
      t.deepEqual(res, { RESULT: ['blue', 'red', 'yellow'], OPTIONS: {} })
    })
})

test('simple DICTIONARY- all entries', t => {
  t.plan(1)
  global[indexName].DICTIONARY().then(res => {
    t.deepEqual(res, {
      RESULT: [
        0,
        9,
        10,
        33,
        222,
        1000000,
        99999999999,
        10000000000000,
        'blue',
        'bmw',
        'red',
        'tesla',
        'volvo',
        'yellow'
      ],
      OPTIONS: {}
    })
  })
})

test('simple DICTIONARY, multiple fields', t => {
  t.plan(1)
  global[indexName]
    .DICTIONARY({
      FIELD: ['colour', 'brand']
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: ['blue', 'bmw', 'red', 'tesla', 'volvo', 'yellow'],
        OPTIONS: {}
      })
    })
})

test('simple DICTIONARY, multiple fields, gte', t => {
  t.plan(1)
  global[indexName]
    .DICTIONARY({
      FIELD: ['colour', 'brand'],
      VALUE: {
        GTE: 'c'
      }
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: ['red', 'tesla', 'volvo', 'yellow'],
        OPTIONS: {}
      })
    })
})

test('simple DICTIONARY, multiple fields, gte + lte', t => {
  t.plan(1)
  global[indexName]
    .DICTIONARY({
      FIELD: ['colour', 'brand'],
      VALUE: {
        GTE: 'c',
        LTE: 'u'
      }
    })
    .then(res => {
      t.deepEqual(res, { RESULT: ['red', 'tesla'], OPTIONS: {} })
    })
})

test('simple DICTIONARY (field: colour)', t => {
  t.plan(1)
  global[indexName]
    .DICTIONARY({
      FIELD: ['colour']
    })
    .then(res => {
      t.deepEqual(res, { RESULT: ['blue', 'red', 'yellow'], OPTIONS: {} })
    })
})

test('simple DICTIONARY (begins with "bl")', t => {
  t.plan(1)
  global[indexName].DICTIONARY('bl').then(res => {
    t.deepEqual(res, { RESULT: ['blue'], OPTIONS: {} })
  })
})

test('FuzzySet test', t => {
  t.plan(1)
  global[indexName].DICTIONARY().then(dict => {
    const fs = FuzzySet()
    dict.RESULT.forEach(d => fs.add(d + ''))
    t.deepEqual(fs.get('blux'), [[0.75, 'blue']])
  })
})

test('simple DICTIONARY, multiple fields, gte + lte', t => {
  t.plan(1)
  global[indexName]
    .DICTIONARY({
      FIELD: ['price'],
      VALUE: {
        GTE: 0,
        LTE: 50
      }
    })
    .then(res => {
      t.deepEqual(res, { RESULT: [0, 9, 10, 33], OPTIONS: {} })
    })
})

test('simple DICTIONARY, timestamped', t => {
  t.plan(1)
  global[indexName].DICTIONARY('yel', { timestamp: '20250217' }).then(res => {
    t.deepEqual(res, {
      RESULT: ['yellow'],
      OPTIONS: { timestamp: '20250217' }
    })
  })
})
