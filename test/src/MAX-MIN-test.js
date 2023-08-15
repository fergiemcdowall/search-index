import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'MAXMIN'
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
      price: 3000
    },
    {
      _id: 1,
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      price: 12000
    },
    {
      _id: 2,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      price: 14000
    },
    {
      _id: 3,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW',
      price: 140000
    },
    {
      _id: 4,
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      price: 1000
    },
    {
      _id: 5,
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      price: 2000
    },
    {
      _id: 6,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW',
      price: 500
    },
    {
      _id: 7,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla',
      price: 5000
    },
    {
      _id: 8,
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla',
      price: 100
    },
    {
      _id: 9,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      price: 1000
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('simple _MAX', t => {
  const { MAX } = global[indexName]
  t.plan(1)
  MAX({ FIELD: 'price' }).then(count => {
    t.equals(count, 140000)
  })
})

test('MAX with GTE', t => {
  const { MAX } = global[indexName]
  t.plan(1)
  MAX({
    FIELD: 'price',
    VALUE: {
      LTE: 4000
    }
  }).then(count => {
    t.equals(count, 3000)
  })
})

test('simple MIN', t => {
  const { MIN } = global[indexName]
  t.plan(1)
  MIN({ FIELD: 'price' }).then(count => {
    t.equals(count, 100)
  })
})

test('simple MAX (JSON)', t => {
  const { MAX } = global[indexName]
  t.plan(1)
  MAX({ FIELD: 'make' }).then(make => {
    t.equals(make, 'volvo')
  })
})

test('simple MIN (JSON)', t => {
  const { MIN } = global[indexName]
  t.plan(1)
  MIN({ FIELD: 'make' }).then(count => {
    t.equals(count, 'bmw')
  })
})
