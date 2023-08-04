import test from 'tape'
const { SearchIndex } = await import(
  '../../src/' + process.env.SI_TEST_ENTRYPOINT
)

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'BUCKETS'

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
      _id: '0',
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: '1',
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: '2',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    },
    {
      _id: '3',
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW'
    },
    {
      _id: '4',
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: '5',
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    },
    {
      _id: '6',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW'
    },
    {
      _id: '7',
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla'
    },
    {
      _id: '8',
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla'
    },
    {
      _id: '9',
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('simple BUCKETS', t => {
  const { BUCKETS } = global[indexName]
  t.plan(1)
  BUCKETS('make:volvo', 'make:bmw', 'make:tesla').then(res => {
    t.deepEqual(res, [
      {
        FIELD: ['make'],
        VALUE: { GTE: 'volvo', LTE: 'volvo' },
        _id: ['4', '5', '8']
      },
      {
        FIELD: ['make'],
        VALUE: { GTE: 'bmw', LTE: 'bmw' },
        _id: ['1', '7', '9']
      },
      {
        FIELD: ['make'],
        VALUE: { GTE: 'tesla', LTE: 'tesla' },
        _id: ['0', '2', '3', '6']
      }
    ])
  })
})

test('simple BUCKETS', t => {
  const { BUCKETS } = global[indexName]
  t.plan(1)
  BUCKETS(
    { FIELD: 'make', VALUE: 'bmw' },
    { FIELD: 'make', VALUE: 'tesla' },
    { FIELD: 'make', VALUE: 'volvo' }
  ).then(res => {
    t.deepEqual(res, [
      {
        FIELD: ['make'],
        VALUE: { GTE: 'bmw', LTE: 'bmw' },
        _id: ['1', '7', '9']
      },
      {
        FIELD: ['make'],
        VALUE: { GTE: 'tesla', LTE: 'tesla' },
        _id: ['0', '2', '3', '6']
      },
      {
        FIELD: ['make'],
        VALUE: { GTE: 'volvo', LTE: 'volvo' },
        _id: ['4', '5', '8']
      }
    ])
  })
})

test('simple DISTINCT.then(BUCKETS)', t => {
  const { DISTINCT, BUCKETS } = global[indexName]
  t.plan(1)
  DISTINCT({ FIELD: 'make' })
    .then(dist => BUCKETS(...dist))
    .then(res => {
      t.deepEqual(res, [
        {
          FIELD: ['make'],
          VALUE: { GTE: 'bmw', LTE: 'bmw' },
          _id: ['1', '7', '9']
        },
        {
          FIELD: ['make'],
          VALUE: { GTE: 'tesla', LTE: 'tesla' },
          _id: ['0', '2', '3', '6']
        },
        {
          FIELD: ['make'],
          VALUE: { GTE: 'volvo', LTE: 'volvo' },
          _id: ['4', '5', '8']
        }
      ])
    })
})

test('QUERY { BUCKETS: ... }', t => {
  const { BUCKETS } = global[indexName]
  t.plan(1)
  BUCKETS('make:bmw', 'make:tesla', 'make:volvo').then(res => {
    t.deepEqual(res, [
      {
        FIELD: ['make'],
        VALUE: { GTE: 'bmw', LTE: 'bmw' },
        _id: ['1', '7', '9']
      },
      {
        FIELD: ['make'],
        VALUE: { GTE: 'tesla', LTE: 'tesla' },
        _id: ['0', '2', '3', '6']
      },
      {
        FIELD: ['make'],
        VALUE: { GTE: 'volvo', LTE: 'volvo' },
        _id: ['4', '5', '8']
      }
    ])
  })
})

test('QUERY { BUCKETS: ... }', t => {
  const { BUCKETS } = global[indexName]
  t.plan(1)
  BUCKETS(
    { FIELD: 'make', VALUE: 'bmw' },
    { FIELD: 'make', VALUE: 'tesla' },
    { FIELD: 'make', VALUE: 'volvo' }
  ).then(res => {
    t.deepEqual(res, [
      {
        FIELD: ['make'],
        VALUE: { GTE: 'bmw', LTE: 'bmw' },
        _id: ['1', '7', '9']
      },
      {
        FIELD: ['make'],
        VALUE: { GTE: 'tesla', LTE: 'tesla' },
        _id: ['0', '2', '3', '6']
      },
      {
        FIELD: ['make'],
        VALUE: { GTE: 'volvo', LTE: 'volvo' },
        _id: ['4', '5', '8']
      }
    ])
  })
})
