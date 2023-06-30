import { SearchIndex } from '../../src/main.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'DOCUMENT_COUNT'

const data = [
  {
    _id: '0',
    make: 'Tesla',
    manufacturer: 'Volvo',
    brand: 'Volvo',
    colour: 'yellow'
  },
  {
    _id: '1',
    make: 'BMW',
    manufacturer: 'Volvo',
    brand: 'Volvo',
    colour: 'red'
  },
  {
    _id: '2',
    make: 'Tesla',
    manufacturer: 'Tesla',
    brand: 'Volvo',
    colour: 'blue'
  },
  {
    _id: '3',
    make: 'Tesla',
    manufacturer: 'Volvo',
    brand: 'BMW',
    colour: 'red'
  },
  {
    _id: '4',
    make: 'Volvo',
    manufacturer: 'Volvo',
    brand: 'Volvo',
    colour: 'red'
  },
  {
    _id: '5',
    make: 'Volvo',
    manufacturer: 'Tesla',
    brand: 'Volvo',
    colour: 'blue'
  },
  {
    _id: '6',
    make: 'Tesla',
    manufacturer: 'Tesla',
    brand: 'BMW',
    colour: 'yellow'
  },
  {
    _id: '7',
    make: 'BMW',
    manufacturer: 'Tesla',
    brand: 'Tesla',
    colour: 'yellow'
  },
  {
    _id: '8',
    make: 'Volvo',
    manufacturer: 'BMW',
    brand: 'Tesla',
    colour: 'blue'
  },
  {
    _id: '9',
    make: 'BMW',
    manufacturer: 'Tesla',
    brand: 'Volvo',
    colour: 'red'
  }
]
const global = {}

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({
      name: indexName,
      storeRawDocs: false,
      storeVectors: true
    })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add data', t => {
  t.plan(1)
  global[indexName].PUT(data.slice(0, 4)).then(res =>
    t.deepEquals(res, [
      { _id: '0', operation: 'PUT', status: 'CREATED' },
      { _id: '1', operation: 'PUT', status: 'CREATED' },
      { _id: '2', operation: 'PUT', status: 'CREATED' },
      { _id: '3', operation: 'PUT', status: 'CREATED' }
    ])
  )
})

test('simple DOCUMENT_COUNT', t => {
  const { DOCUMENT_COUNT } = global[indexName]
  t.plan(1)
  DOCUMENT_COUNT().then(count => {
    t.equals(count, 4)
  })
})

test('add some more docs, some UPDATED and some CREATED', t => {
  t.plan(1)
  global[indexName].PUT(data.slice(2, 6)).then(res =>
    t.deepEquals(res, [
      { _id: '2', operation: 'PUT', status: 'UPDATED' },
      { _id: '3', operation: 'PUT', status: 'UPDATED' },
      { _id: '4', operation: 'PUT', status: 'CREATED' },
      { _id: '5', operation: 'PUT', status: 'CREATED' }
    ])
  )
})

test('simple DOCUMENT_COUNT', t => {
  const { DOCUMENT_COUNT } = global[indexName]
  t.plan(1)
  DOCUMENT_COUNT().then(count => {
    t.equals(count, 6)
  })
})

test('can DELETE data', t => {
  t.plan(1)
  global[indexName].DELETE('3', '4', '7').then(res =>
    t.deepEquals(res, [
      { _id: '3', operation: 'DELETE', status: 'DELETED' },
      { _id: '4', operation: 'DELETE', status: 'DELETED' },
      { _id: '7', operation: 'DELETE', status: 'FAILED' }
    ])
  )
})

test('simple DOCUMENT_COUNT', t => {
  const { DOCUMENT_COUNT } = global[indexName]
  t.plan(1)
  DOCUMENT_COUNT().then(count => {
    t.equals(count, 4)
  })
})
