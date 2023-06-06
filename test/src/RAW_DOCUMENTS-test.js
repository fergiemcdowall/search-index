const si = require('../../')
const { EntryStream } = require('level-read-stream')
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
      { _id: 0, status: 'CREATED', operation: 'PUT' },
      { _id: 1, status: 'CREATED', operation: 'PUT' },
      { _id: 2, status: 'CREATED', operation: 'PUT' }
    ])
  )
})

test('DOC_RAWs are inserted as expected', t => {
  const indexEntries = [
    {
      key: ['DOC_RAW', 0],
      value: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', 1],
      value: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', 2],
      value: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo' }
    }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName].INDEX.STORE, {
    gte: ['DOC_RAW', null],
    lte: ['DOC_RAW', undefined],
    ...global[indexName].INDEX.LEVEL_OPTIONS
  }).on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})

test('can add raw document', t => {
  t.plan(1)
  global[indexName]
    .PUT_RAW(
      [
        {
          text: 'this is an altered raw doc'
        }
      ],
      [1]
    )
    .then(response =>
      t.deepEquals(response, [{ _id: 1, status: 'OK', operation: '_PUT_RAW' }])
    )
})

test('Verify that PUT_RAW has updated the raw document', t => {
  const indexEntries = [
    {
      key: ['DOC_RAW', 0],
      value: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', 1],
      value: { text: 'this is an altered raw doc' }
    },
    {
      key: ['DOC_RAW', 2],
      value: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo' }
    }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName].INDEX.STORE, {
    gte: ['DOC_RAW', null],
    lte: ['DOC_RAW', undefined],
    ...global[indexName].INDEX.LEVEL_OPTIONS
  }).on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})

test('can add raw document with external API (no underscore)', t => {
  t.plan(1)
  global[indexName]
    .PUT_RAW(
      [
        {
          text: 'this is an altered raw doc for 2'
        }
      ],
      [2]
    )
    .then(response =>
      t.deepEquals(response, [{ _id: 2, status: 'OK', operation: '_PUT_RAW' }])
    )
})

test('Verify that PUT_RAW has created an appropriate index', t => {
  const indexEntries = [
    {
      key: ['DOC_RAW', 0],
      value: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', 1],
      value: { text: 'this is an altered raw doc' }
    },
    {
      key: ['DOC_RAW', 2],
      value: { text: 'this is an altered raw doc for 2' }
    }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName].INDEX.STORE, {
    gte: ['DOC_RAW', null],
    lte: ['DOC_RAW', undefined],
    ...global[indexName].INDEX.LEVEL_OPTIONS
  }).on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})
