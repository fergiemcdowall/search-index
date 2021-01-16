const si = require('../../')
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
  global[indexName].PUT(
    carData, {
      storeRawDocs: false
    }).then(response =>
    t.deepEquals(response, [
      { _id: '0', status: 'CREATED', operation: 'PUT' },
      { _id: '1', status: 'CREATED', operation: 'PUT' }
    ])
  )
})

test('Verify that an appropriate index has been created', t => {
  const indexEntries = [
    { key: 'brand:volvo#1.00', value: ['0', '1'] },
    { key: 'make:bmw#1.00', value: ['1'] },
    { key: 'make:tesla#1.00', value: ['0'] },
    { key: 'manufacturer:volvo#1.00', value: ['0', '1'] },
    { key: '￮DOCUMENT_COUNT￮', value: 2 },
    { key: '￮DOC_RAW￮0￮', value: {} },
    { key: '￮DOC_RAW￮1￮', value: {} },
    { key: '￮FIELD￮brand￮', value: 'brand' },
    { key: '￮FIELD￮make￮', value: 'make' },
    { key: '￮FIELD￮manufacturer￮', value: 'manufacturer' }
  ]
  t.plan(indexEntries.length)
  global[indexName].INDEX.STORE.createReadStream().on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})
