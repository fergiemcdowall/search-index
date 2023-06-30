import { SearchIndex } from '../../src/main.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_NOT'
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
      brand: 'Volvo'
    },
    {
      _id: 1,
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: 2,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    },
    {
      _id: 3,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW'
    },
    {
      _id: 4,
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: 5,
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    },
    {
      _id: 6,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW'
    },
    {
      _id: 7,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla'
    },
    {
      _id: 8,
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla'
    },
    {
      _id: 9,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('simple _NOT', t => {
  const { _NOT } = global[indexName]
  t.plan(1)
  _NOT('make:volvo', 'brand:tesla').then(res => {
    t.deepEqual(res, [
      { _id: 4, _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] },
      { _id: 5, _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] }
    ])
  })
})

test('simple _NOT', t => {
  const { _NOT } = global[indexName]
  t.plan(1)
  _NOT('brand:volvo', 'make:bmw').then(res => {
    t.deepEqual(res, [
      { _id: 0, _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }] },
      { _id: 2, _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }] },
      { _id: 4, _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }] },
      { _id: 5, _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }] }
    ])
  })
})

test('simple _NOT with OR clause', t => {
  const { _OR, _NOT } = global[indexName]
  t.plan(1)
  _NOT(_OR(['make:bmw', 'make:volvo']), 'brand:tesla').then(res => {
    t.deepEqual(res, [
      { _id: 1, _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] },
      { _id: 9, _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] },
      { _id: 4, _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] },
      { _id: 5, _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] }
    ])
  })
})

test('simple NOT', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    NOT: {
      INCLUDE: 'make:volvo',
      EXCLUDE: 'brand:tesla'
    }
  }).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 4,
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        { _id: 5, _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] }
      ],
      RESULT_LENGTH: 2
    })
  })
})
