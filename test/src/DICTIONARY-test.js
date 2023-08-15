import FuzzySet from 'fuzzyset'
import test from 'tape'

import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_DICTIONARY'
const global = {}

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({ name: indexName })
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
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY({
    FIELD: ['colour']
  }).then(res => {
    t.deepEqual(res, ['blue', 'red', 'yellow'])
  })
})

test('simple DICTIONARY- all entries', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY().then(res => {
    t.deepEqual(res, [
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
    ])
  })
})

test('simple DICTIONARY, multiple fields', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY({
    FIELD: ['colour', 'brand']
  }).then(res => {
    t.deepEqual(res, ['blue', 'bmw', 'red', 'tesla', 'volvo', 'yellow'])
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
    t.deepEqual(res, ['red', 'tesla', 'volvo', 'yellow'])
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
    t.deepEqual(res, ['red', 'tesla'])
  })
})

test('simple DICTIONARY (field: colour)', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY({
    FIELD: ['colour']
  }).then(res => {
    t.deepEqual(res, ['blue', 'red', 'yellow'])
  })
})

test('simple DICTIONARY (begins with "bl")', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY('bl').then(res => {
    t.deepEqual(res, ['blue'])
  })
})

test('FuzzySet test', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY().then(dict => {
    const fs = FuzzySet()
    dict.forEach(d => fs.add(d + ''))
    t.deepEqual(fs.get('blux'), [[0.75, 'blue']])
  })
})

test('simple DICTIONARY, multiple fields, gte + lte', t => {
  const { DICTIONARY } = global[indexName]
  t.plan(1)
  DICTIONARY({
    FIELD: ['price'],
    VALUE: {
      GTE: 0,
      LTE: 50
    }
  }).then(res => {
    t.deepEqual(res, [0, 9, 10, 33])
  })
})
