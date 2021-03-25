const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'PUT_RAW_DOCUMENTS'

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
  },
  {
    _id: 2,
    make: 'Tesla',
    manufacturer: 'Tesla',
    brand: 'Volvo'
  }
]

test('create a search index', t => {
  t.plan(1)
  si({ name: indexName }).then(db => {
    global[indexName] = db
    t.pass('ok')
  })
})

test('can add data', t => {
  t.plan(1)
  global[indexName].PUT(carData).then(response =>
    t.deepEquals(response, [
      { _id: '0', status: 'CREATED', operation: 'PUT' },
      { _id: '1', status: 'CREATED', operation: 'PUT' },
      { _id: '2', status: 'CREATED', operation: 'PUT' }
    ])
  )
})

test('Verify that PUT has created an appropriate index (_PUT_1)', t => {
  const indexEntries = [
    { key: 'brand:volvo#1.00', value: ['0', '1', '2'] },
    { key: 'make:bmw#1.00', value: ['1'] },
    { key: 'make:tesla#1.00', value: ['0', '2'] },
    { key: 'manufacturer:tesla#1.00', value: ['2'] },
    { key: 'manufacturer:volvo#1.00', value: ['0', '1'] },
    { key: '￮DOCUMENT_COUNT￮', value: 3 },
    { key: '￮DOC_RAW￮0￮', value: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' } },
    { key: '￮DOC_RAW￮1￮', value: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo' } },
    { key: '￮DOC_RAW￮2￮', value: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo' } },
    { key: '￮DOC￮0￮', value: { _id: '0', make: ['tesla#1.00'], manufacturer: ['volvo#1.00'], brand: ['volvo#1.00'] } },
    { key: '￮DOC￮1￮', value: { _id: '1', make: ['bmw#1.00'], manufacturer: ['volvo#1.00'], brand: ['volvo#1.00'] } },
    { key: '￮DOC￮2￮', value: { _id: '2', make: ['tesla#1.00'], manufacturer: ['tesla#1.00'], brand: ['volvo#1.00'] } },
    { key: '￮FIELD￮brand￮', value: 'brand' },
    { key: '￮FIELD￮make￮', value: 'make' },
    { key: '￮FIELD￮manufacturer￮', value: 'manufacturer' }
  ]
  t.plan(indexEntries.length)
  global[indexName].INDEX.STORE.createReadStream({ lt: '￮￮' }).on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})

test('can add raw document', t => {
  t.plan(1)
  global[indexName].PUT_RAW([{
    _id: '1',
    text: 'this is an altered raw doc'
  }]).then(response =>
    t.deepEquals(response, [
      { _id: '1', status: 'OK', operation: '_PUT_RAW' }
    ])
  )
})

test('Verify that PUT_RAW has created an appropriate index', t => {
  const indexEntries = [
    { key: 'brand:volvo#1.00', value: ['0', '1', '2'] },
    { key: 'make:bmw#1.00', value: ['1'] },
    { key: 'make:tesla#1.00', value: ['0', '2'] },
    { key: 'manufacturer:tesla#1.00', value: ['2'] },
    { key: 'manufacturer:volvo#1.00', value: ['0', '1'] },
    { key: '￮DOCUMENT_COUNT￮', value: 3 },
    { key: '￮DOC_RAW￮0￮', value: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' } },
    { key: '￮DOC_RAW￮1￮', value: { _id: '1', text: 'this is an altered raw doc' } },
    { key: '￮DOC_RAW￮2￮', value: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo' } },
    { key: '￮DOC￮0￮', value: { _id: '0', make: ['tesla#1.00'], manufacturer: ['volvo#1.00'], brand: ['volvo#1.00'] } },
    { key: '￮DOC￮1￮', value: { _id: '1', make: ['bmw#1.00'], manufacturer: ['volvo#1.00'], brand: ['volvo#1.00'] } },
    { key: '￮DOC￮2￮', value: { _id: '2', make: ['tesla#1.00'], manufacturer: ['tesla#1.00'], brand: ['volvo#1.00'] } },
    { key: '￮FIELD￮brand￮', value: 'brand' },
    { key: '￮FIELD￮make￮', value: 'make' },
    { key: '￮FIELD￮manufacturer￮', value: 'manufacturer' }
  ]
  t.plan(indexEntries.length)
  global[indexName].INDEX.STORE.createReadStream({ lt: '￮￮' }).on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})

test('can add raw document with external API (no underscore)', t => {
  t.plan(1)
  global[indexName].PUT_RAW([{
    _id: '2',
    text: 'this is an altered raw doc for 2'
  }]).then(response =>
    t.deepEquals(response, [
      { _id: '2', status: 'OK', operation: '_PUT_RAW' }
    ])
  )
})

test('Verify that PUT_RAW has created an appropriate index', t => {
  const indexEntries = [
    { key: 'brand:volvo#1.00', value: ['0', '1', '2'] },
    { key: 'make:bmw#1.00', value: ['1'] },
    { key: 'make:tesla#1.00', value: ['0', '2'] },
    { key: 'manufacturer:tesla#1.00', value: ['2'] },
    { key: 'manufacturer:volvo#1.00', value: ['0', '1'] },
    { key: '￮DOCUMENT_COUNT￮', value: 3 },
    { key: '￮DOC_RAW￮0￮', value: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' } },
    { key: '￮DOC_RAW￮1￮', value: { _id: '1', text: 'this is an altered raw doc' } },
    { key: '￮DOC_RAW￮2￮', value: { _id: '2', text: 'this is an altered raw doc for 2' } },
    { key: '￮DOC￮0￮', value: { _id: '0', make: ['tesla#1.00'], manufacturer: ['volvo#1.00'], brand: ['volvo#1.00'] } },
    { key: '￮DOC￮1￮', value: { _id: '1', make: ['bmw#1.00'], manufacturer: ['volvo#1.00'], brand: ['volvo#1.00'] } },
    { key: '￮DOC￮2￮', value: { _id: '2', make: ['tesla#1.00'], manufacturer: ['tesla#1.00'], brand: ['volvo#1.00'] } },
    { key: '￮FIELD￮brand￮', value: 'brand' },
    { key: '￮FIELD￮make￮', value: 'make' },
    { key: '￮FIELD￮manufacturer￮', value: 'manufacturer' }
  ]
  t.plan(indexEntries.length)
  global[indexName].INDEX.STORE.createReadStream({ lt: '￮￮' }).on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})
