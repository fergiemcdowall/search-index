import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'DISTINCT'
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
      colour: 'yellow'
    },
    {
      _id: 1,
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      colour: 'red'
    },
    {
      _id: 2,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colour: 'blue'
    },
    {
      _id: 3,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW',
      colour: 'red'
    },
    {
      _id: 4,
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      colour: 'red'
    },
    {
      _id: 5,
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colour: 'blue'
    },
    {
      _id: 6,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW',
      colour: 'yellow'
    },
    {
      _id: 7,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla',
      colour: 'yellow'
    },
    {
      _id: 8,
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla',
      colour: 'blue'
    },
    {
      _id: 9,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colour: 'red'
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('simple DISTINCT', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    FIELD: 'colour'
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'blue' },
      { FIELD: 'colour', VALUE: 'red' },
      { FIELD: 'colour', VALUE: 'yellow' }
    ])
  })
})

test('simple DISTINCT with range', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    FIELD: 'colour',
    VALUE: {
      GTE: 'a',
      LTE: 'c'
    }
  }).then(res => {
    t.deepEqual(res, [{ FIELD: 'colour', VALUE: 'blue' }])
  })
})

test('simple DISTINCT with range', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    FIELD: 'colour',
    VALUE: {
      GTE: 'c'
    }
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'red' },
      { FIELD: 'colour', VALUE: 'yellow' }
    ])
  })
})

test('simple DISTINCT with range', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    FIELD: 'colour',
    VALUE: {
      LTE: 'c'
    }
  }).then(res => {
    t.deepEqual(res, [{ FIELD: 'colour', VALUE: 'blue' }])
  })
})

test('simple DISTINCT', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    FIELD: 'colour'
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'blue' },
      { FIELD: 'colour', VALUE: 'red' },
      { FIELD: 'colour', VALUE: 'yellow' }
    ])
  })
})

test('simple DISTINCT', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    VALUE: 'red'
  }).then(res => {
    t.deepEqual(res, [{ FIELD: 'colour', VALUE: 'red' }])
  })
})

test('simple DISTINCT', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    VALUE: 'volvo'
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'brand', VALUE: 'volvo' },
      { FIELD: 'make', VALUE: 'volvo' },
      { FIELD: 'manufacturer', VALUE: 'volvo' }
    ])
  })
})

test('simple DISTINCT', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    FIELD: 'brand'
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'brand', VALUE: 'bmw' },
      { FIELD: 'brand', VALUE: 'tesla' },
      { FIELD: 'brand', VALUE: 'volvo' }
    ])
  })
})

test('DISTINCT on 2 fields', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT({
    FIELD: ['brand', 'colour']
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'brand', VALUE: 'bmw' },
      { FIELD: 'brand', VALUE: 'tesla' },
      { FIELD: 'brand', VALUE: 'volvo' },
      { FIELD: 'colour', VALUE: 'blue' },
      { FIELD: 'colour', VALUE: 'red' },
      { FIELD: 'colour', VALUE: 'yellow' }
    ])
  })
})

test('DISTINCT on 2 fields with GTE/LTE', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT(
    {
      FIELD: ['brand'],
      VALUE: {
        GTE: 'f'
      }
    },
    {
      FIELD: ['colour'],
      VALUE: {
        LTE: 'x'
      }
    }
  ).then(res => {
    t.deepEqual(res, [
      { FIELD: 'brand', VALUE: 'tesla' },
      { FIELD: 'brand', VALUE: 'volvo' },
      { FIELD: 'colour', VALUE: 'blue' },
      { FIELD: 'colour', VALUE: 'red' }
    ])
  })
})

test('DISTINCT from string', t => {
  const { DISTINCT } = global[indexName]
  t.plan(1)
  DISTINCT('tes').then(res => {
    t.deepEqual(res, [
      { FIELD: 'brand', VALUE: 'tesla' },
      { FIELD: 'make', VALUE: 'tesla' },
      { FIELD: 'manufacturer', VALUE: 'tesla' }
    ])
  })
})
