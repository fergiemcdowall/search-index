const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const exportingIndexName = sandbox + 'EXPORT'
const importingIndexName = sandbox + 'IMPORT'

let exportedIndex = null

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
  { key: 'brand:volvo#1.00', value: ['0', '1'] },
  { key: 'make:bmw#1.00', value: ['1'] },
  { key: 'make:tesla#1.00', value: ['0'] },
  { key: 'manufacturer:volvo#1.00', value: ['0', '1'] },
  { key: '￮DOCUMENT_COUNT￮', value: 2 },
  {
    key: '￮DOC_RAW￮0￮',
    value: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' }
  },
  {
    key: '￮DOC_RAW￮1￮',
    value: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo' }
  },
  { key: '￮FIELD￮brand￮', value: 'brand' },
  { key: '￮FIELD￮make￮', value: 'make' },
  { key: '￮FIELD￮manufacturer￮', value: 'manufacturer' }
]

test('create a search index for exporting from', t => {
  t.plan(1)
  si({ name: exportingIndexName }).then(db => {
    global[exportingIndexName] = db
    t.pass('ok')
  })
})

test('can add data', t => {
  t.plan(1)
  global[exportingIndexName].PUT(carData).then(response =>
    t.deepEquals(response, [
      { _id: '0', status: 'CREATED', operation: 'PUT' },
      { _id: '1', status: 'CREATED', operation: 'PUT' }
    ])
  )
})

test('can export data', t => {
  t.plan(1)
  global[exportingIndexName]
    .EXPORT()
    .then(index => {
      exportedIndex = index
      index.pop() // remove timestamp
      index.pop() // remove timestamp
      t.deepEquals(expectedIndex, index)
    })
})

test('create a search index for importing to', t => {
  t.plan(1)
  si({ name: importingIndexName }).then(db => {
    global[importingIndexName] = db
    t.pass('ok')
  })
})

test('can add data that will be overwritten', t => {
  t.plan(1)
  global[importingIndexName].PUT([{
    _id: '999',
    text: 'this doc will be removed during IMPORT'
  }]).then(response =>
    t.deepEquals(response, [
      { _id: '999', status: 'CREATED', operation: 'PUT' }
    ])
  )
})

test('can import data', t => {
  t.plan(1)
  console.log('HEEERE')
  global[importingIndexName]
    .IMPORT(exportedIndex)
    .then(() => t.ok('imported'))
})

test('verify structure of imported data', t => {
  t.plan(1)
  global[importingIndexName]
    .EXPORT()
    .then(index => {
      exportedIndex = index
      t.deepEqual(index, expectedIndex)
    })
})
