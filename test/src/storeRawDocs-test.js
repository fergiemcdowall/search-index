const si = require('../../')
const { EntryStream } = require('level-read-stream')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'storeRawDocs'

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

test('create a search index', t => {
  t.plan(1)
  si({
    name: indexName
  }).then(db => {
    global[indexName] = db
    t.pass('ok')
  })
})

test('can add data', t => {
  t.plan(1)
  global[indexName]
    .PUT(carData, {
      storeRawDocs: false
    })
    .then(response =>
      t.deepEquals(response, [
        { _id: 0, status: 'CREATED', operation: 'PUT' },
        { _id: 1, status: 'CREATED', operation: 'PUT' }
      ])
    )
})

test('Verify that an appropriate index has been created (no raw docs)', t => {
  const indexEntries = [
    {
      key: ['CREATED_WITH'],
      value: 'search-index@' + require('../../package.json').version
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
    { key: ['DOC_RAW', 0], value: {} },
    { key: ['DOC_RAW', 1], value: {} },
    { key: ['FIELD', 'brand'], value: 'brand' },
    { key: ['FIELD', 'make'], value: 'make' },
    { key: ['FIELD', 'manufacturer'], value: 'manufacturer' },
    { key: ['IDX', 'brand', ['volvo', '1.00']], value: [0, 1] },
    { key: ['IDX', 'make', ['bmw', '1.00']], value: [1] },
    { key: ['IDX', 'make', ['tesla', '1.00']], value: [0] },
    {
      key: ['IDX', 'manufacturer', ['volvo', '1.00']],
      value: [0, 1]
    }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName].INDEX.STORE, { lt: ['~'], ...global[indexName].INDEX.LEVEL_OPTIONS }).on(
    'data',
    d => {
      t.deepEquals(d, indexEntries.shift())
    }
  )
})
