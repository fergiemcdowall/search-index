import test from 'tape'
import { SearchIndex } from 'search-index'
import { packageVersion } from '../../src/version.js'

let exportedIndex = null // holds a text export of an index

const sandbox = 'test/sandbox/'
const exportingIndexName = sandbox + 'EXPORT'
const importingIndexName = sandbox + 'IMPORT'
const global = {}
const carData = [
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
  }
]

const expectedIndex = [
  {
    key: ['CREATED_WITH'],
    value: 'search-index@' + packageVersion
  },
  {
    key: ['DOC', 0],
    value: {
      _id: 0,
      make: [['tesla', '1.00']],
      manufacturer: [['volvo', '1.00']],
      brand: [['volvo', '1.00']]
    }
  },
  {
    key: ['DOC', 1],
    value: {
      _id: 1,
      make: [['bmw', '1.00']],
      manufacturer: [['volvo', '1.00']],
      brand: [['volvo', '1.00']]
    }
  },
  { key: ['DOCUMENT_COUNT'], value: 2 },
  {
    key: ['DOC_RAW', 0],
    value: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' }
  },
  {
    key: ['DOC_RAW', 1],
    value: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo' }
  },
  { key: ['FIELD', 'brand'], value: 'brand' },
  { key: ['FIELD', 'make'], value: 'make' },
  { key: ['FIELD', 'manufacturer'], value: 'manufacturer' },
  { key: ['IDX', 'brand', ['volvo', '1.00']], value: [0, 1] },
  { key: ['IDX', 'make', ['bmw', '1.00']], value: [1] },
  { key: ['IDX', 'make', ['tesla', '1.00']], value: [0] },
  { key: ['IDX', 'manufacturer', ['volvo', '1.00']], value: [0, 1] }
]

test('create a search index for exporting from', t => {
  t.plan(1)
  try {
    global[exportingIndexName] = new SearchIndex({
      name: exportingIndexName
    })
    t.ok(global[exportingIndexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add data', t => {
  t.plan(1)
  global[exportingIndexName].PUT(carData).then(response =>
    t.deepEquals(response, [
      { _id: 0, status: 'CREATED', operation: 'PUT' },
      { _id: 1, status: 'CREATED', operation: 'PUT' }
    ])
  )
})

test('can search and get hits', t => {
  t.plan(1)
  global[exportingIndexName].SEARCH(['bmw']).then(res => {
    t.deepEquals(res, {
      RESULT: [
        {
          _id: 1,
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }],
          _score: 1.1
        }
      ],
      RESULT_LENGTH: 1,
      PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
    })
  })
})

test('can export data', t => {
  t.plan(1)
  global[exportingIndexName].EXPORT().then(index => {
    exportedIndex = index
    index.pop() // remove timestamps
    index.pop() // remove timestamps
    t.deepEquals(index, expectedIndex)
  })
})

test('create a search index for importing to', t => {
  t.plan(1)
  try {
    global[importingIndexName] = new SearchIndex({
      name: importingIndexName
    })
    t.ok(global[importingIndexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add data that will be overwritten', t => {
  t.plan(1)
  global[importingIndexName]
    .PUT([
      {
        _id: '999',
        text: 'this doc will be removed during IMPORT'
      }
    ])
    .then(response =>
      t.deepEquals(response, [
        { _id: '999', status: 'CREATED', operation: 'PUT' }
      ])
    )
})

test('search for "bmw" will _not_ give hits (but will create a cache record)', t => {
  t.plan(1)
  global[importingIndexName].SEARCH(['bmw']).then(res => {
    t.deepEquals(res, {
      RESULT: [],
      RESULT_LENGTH: 0,
      PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 0, DOC_OFFSET: 0 }
    })
  })
})

test('can import data', t => {
  t.plan(1)
  global[importingIndexName].IMPORT(exportedIndex).then(() => t.ok('imported'))
})

test('search for "bmw" _will_ give hits since cache is overwritten', t => {
  t.plan(1)
  global[importingIndexName].SEARCH(['bmw']).then(res => {
    t.deepEquals(res, {
      RESULT: [
        {
          _id: 1,
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }],
          _score: 1.1
        }
      ],
      RESULT_LENGTH: 1,
      PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
    })
  })
})

test('verify structure of imported data', t => {
  t.plan(1)
  global[importingIndexName].EXPORT().then(index => {
    exportedIndex = index
    t.deepEqual(index, expectedIndex)
  })
})
